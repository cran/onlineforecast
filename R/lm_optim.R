# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lm_optim


#' Optimize parameters (transformation stage) of LM model
#'
#' This is a wrapper for \code{\link{optim}} to enable easy use of bounds and caching in the optimization.
#' 
#' @title Optimize parameters for onlineforecast model fitted with LM
#' @param model The onlineforecast model, including inputs, output, kseq, p
#' @param data The data.list including the variables used in the model.
#' @param scorefun The function to be score used for calculating the score to be optimized.
#' @param cachedir A character specifying the path (and prefix) of the cache file name. If set to \code{""}, then no cache will be loaded or written. See \url{https://onlineforecasting.org/vignettes/nice-tricks.html} for examples.
#' @param printout A logical determining if the score function is printed out in each iteration of the optimization.
#' @param method The method argument for \code{\link{optim}}.
#' @param ... Additional parameters to \code{\link{optim}}
#' @return Result object of optim().
#' Parameters resulting from the optimization can be found from \code{result$par}
#' @seealso \code{link{optim}} for how to control the optimization and \code{\link{rls_optim}} which works very similarly.
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "lp(Ta, a1=0.9)",
#'                  mu = "one()")
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#' 
#' # Now we can fit the model and get the score, as it is
#' lm_fit(model=model, data=D, scorefun=rmse, returnanalysis=FALSE)
#' # Or we can change the low-pass filter coefficient
#' lm_fit(c(Ta__a1=0.99), model, D, rmse, returnanalysis=FALSE)
#'
#' # This could be passed to optim() (or any optimizer).
#' # See \code{forecastmodel$insert_prm()} for more details.
#' optim(c(Ta__a1=0.98), lm_fit, model=model, data=D, scorefun=rmse, returnanalysis=FALSE,
#'       lower=c(Ta__a1=0.4), upper=c(Ta__a1=0.999), method="L-BFGS-B")
#'
#' # lm_optim is simply a helper it makes using bounds easiere and enables caching of the results
#' # First add bounds for lambda (lower, init, upper)
#' model$add_prmbounds(Ta__a1 = c(0.4, 0.98, 0.999))
#'
#' # Now the same optimization as above can be done by
#' val <- lm_optim(model, D)
#' val
#'
#'
#' @importFrom stats optim
#' @export
lm_optim <- function(model, data, scorefun = rmse, cachedir="", printout=TRUE, method="L-BFGS-B", ...){
    ## Take the parameters bounds from the parameter bounds set in the model
    init <- model$get_prmbounds("init")
    lower <- model$get_prmbounds("lower")
    upper <- model$get_prmbounds("upper")
    # If bounds are NA, then set
    if(any(is.na(lower))){ lower[is.na(lower)] <- -Inf}
    if(any(is.na(upper))){ lower[is.na(upper)] <- Inf}

    ## Caching the results based on some of the function arguments
    if(cachedir != ""){
        ## Have to insert the parameters in the expressions
        model$insert_prm(init)
        ## Give all the elements to calculate the unique cache name
        cnm <- cache_name(lm_fit, lm_optim, model$outputrange, model$regprm, model$transform_data(data),
                          data[[model$output]], scorefun, init, lower, upper, cachedir = cachedir)
        ## Maybe load the cached result
        if(file.exists(cnm)){ return(readRDS(cnm)) }
    }

    ## Run the optimization
    res <- optim(par = init,
                 fn = lm_fit,
                 model = model,
                 data = data,
                 scorefun = scorefun,
                 printout = printout,
                 returnanalysis = FALSE,
                 lower = lower,
                 upper = upper,
                 method = method,
                 ...)
    ## Save the result in the cachedir
    if(cachedir != ""){ cache_save(res, cnm) }
    ## Return the result
    return(res)
}
