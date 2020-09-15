# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?rls_optim


#' Optimize parameters (transformation stage) of RLS model
#'
#' This is a wrapper for \code{\link{optim}} to enable easy use of bounds and caching in the optimization.
#'
#' One smart trick, is to cache the optimization results. Caching can be done by providing a path to the
#' \code{cachedir} argument (relative to the current working directory).
#' E.g. \code{rls_optim(model, D, cachedir="cache")} will write a file in the folder 'cache', such that
#' next time the same call is carried out, then the file is read instead of running the optimization again.
#' See the example in url{https://onlineforecasting.org/vignettes/nice-tricks.html}.
#' 
#' @title Optimize parameters for onlineforecast model fitted with RLS
#' @param model The onlineforecast model, including inputs, output, kseq, p
#' @param data The data.list including the variables used in the model.
#' @param scorefun The function to be score used for calculating the score to be optimized.
#' @param cachedir A character specifying the path (and prefix) of the cache file name. If set to \code{""}, then no cache will be loaded or written. See \url{https://onlineforecasting.org/vignettes/nice-tricks.html} for examples.
#' @param printout A logical determining if the score function is printed out in each iteration of the optimization.
#' @param method The method argument for \code{\link{optim}}.
#' @param ... Additional parameters to \code{\link{optim}}
#' @return Result object of optim().
#' Parameters resulting from the optimization can be found from \code{result$par}
#' @seealso \code{link{optim}} for how to control the optimization.
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "Ta", mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#'
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#' # Now we can fit the model and get the score, as it is
#' rls_fit(model=model, data=D, scorefun=rmse, returnanalysis=FALSE)
#' # Or we can change the lambda
#' rls_fit(c(lambda=0.9), model, D, rmse, returnanalysis=FALSE)
#'
#' # This could be passed to optim() (or any optimizer, see forecastmodel$insert_prm()).
#' optim(c(lambda=0.98), rls_fit, model=model, data=D, scorefun=rmse, returnanalysis=FALSE,
#'       lower=c(lambda=0.9), upper=c(lambda=0.999), method="L-BFGS-B")
#'
#' # rls_optim is simply a helper, it's makes using bounds easiere and enables caching of the results
#' # First add bounds for lambda (lower, init, upper)
#' model$add_prmbounds(lambda = c(0.9, 0.98, 0.999))
#'
#' # Now the same optimization as above can be done by
#' val <- rls_optim(model, D)
#' val
#' 
#' 
#' @export
rls_optim <- function(model, data, scorefun = rmse, cachedir="", printout=TRUE, method="L-BFGS-B", ...){
    # Take the parameters bounds from the parameter bounds set in the model
    init <- model$get_prmbounds("init")
    lower <- model$get_prmbounds("lower")
    upper <- model$get_prmbounds("upper")
    # If bounds are NA, then set
    if(any(is.na(lower))){ lower[is.na(lower)] <- -Inf}
    if(any(is.na(upper))){ lower[is.na(upper)] <- Inf}

    # Caching the results based on some of the function arguments
    if(cachedir != ""){
        # Have to insert the parameters in the expressions to get the right state of the model for unique checksum
        model$insert_prm(init)
        # Give all the elements needed to calculate the unique cache name
        # This is maybe smarter, don't have to calculate the transformation of the data: cnm <- cache_name(model$regprm, getse(model$inputs, nms="expr"), model$output, model$prmbounds, model$kseq, data, objfun, init, lower, upper, cachedir = cachedir)
        # Have to reset the state first to remove dependency of previous calls
        model$reset_state()
        cnm <- cache_name(rls_fit, rls_optim, model$outputrange, model$regprm, model$transform_data(data), data[[model$output]], scorefun, init, lower, upper, cachedir = cachedir)
        # Maybe load the cached result
        if(file.exists(cnm)){ return(readRDS(cnm)) }
    }

    # Run the optimization
    res <- optim(par = init,
                 fn = rls_fit,
                 # Parameters to pass to rls_fit
                 model = model,
                 data = data,
                 scorefun = scorefun,
                 printout = printout,
                 returnanalysis = FALSE,
                 # Parameters to pass to optim
                 lower = lower,
                 upper = upper,
                 method =  method,
                 ...)
    
    # Save the result in the cachedir
    if(cachedir != ""){ cache_save(res, cnm) }
    # Return the result
    return(res)
}
