# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?rls_fit

#' This function fits the onlineforecast model to the data and returns either: model validation data or just the score value.
#'
#' 
#' This function has three main purposes (in the examples these three are demonstrated in the examples):
#' 
#' - Returning model validation data, such as residuals and recursive estimated parameters.
#' 
#' - For optimizing the parameters using an R optimizer function. The parameters to optimize for is given in \code{prm}
#'
#' - Fitting a model to data and saving the final state in the model object (such that from that point the model can be updated recursively as new data is received).
#' 
#' Note, if the \code{scorefun} is given the \code{data$scoreperiod} must be set to (int or logical) define which points to be evaluated in the scorefun.
#' 
#' @title Fit an onlineforecast model with Recursive Least Squares (RLS).
#' @param prm vector with the parameters for fitting. Deliberately as the first element to be able to use \code{\link{optim}} or other optimizer. If NA then the model will be fitted with the current values in the input expressions, see examples.
#' @param model as an object of class forecastmodel: The model to be fitted.
#' @param data as a data.list with the data to fit the model on.
#' @param scorefun as a function (optional), default is \code{\link{rmse}}. If the score function is given it will be applied to the residuals of each horizon (only data$scoreperiod is included).
#' @param returnanalysis as a logical. If FALSE then the sum of the scoreval on all horizons are returned, if TRUE a list with values for analysis.
#' @param runcpp logical: If true the c++ implementation of RLS is run, if false the R implementation is run (slower).
#' @param printout logical: If TRUE the offline parameters and the score function value are printed.
#' @return Depends on:
#' 
#'     - If \code{returnanalysis} is TRUE a list containing:
#' 
#'         * \code{Yhat}: data.frame with forecasts for \code{model$kseq} horizons.
#'
#'         * \code{model}: The forecastmodel object cloned deep, so can be modified without changing the original object.
#' 
#'         * \code{data}: data.list with the data used, see examples on how to obtain the transformed data.
#'
#'         * \code{Lfitval}: list with RLS coefficients in a data.frame for each horizon, use \code{\link{plot_ts.rls_fit}} to plot them and to obtain them as a data.frame for each coefficient.
#'
#'         * \code{scoreval}: data.frame with the scorefun result on each horizon (only scoreperiod is included).
#'
#'     - If \code{returnanalysis} is FALSE (and \code{scorefun} is given): The sum of the score function on all horizons (specified with model$kseq).
#'
#' @seealso
#' For optimizing parameters \code{\link{rls_optim}()}, for summary \code{summary.rls_fit}, for plotting \code{\link{plot_ts.rls_fit}()}, and the other functions starting with 'rls_'.
#'
#' @examples
#'
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$output <- "y"
#' model$add_inputs(Ta = "Ta",
#' 		    mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#'
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#'
#' # Now we can fit the model with RLS and get the model validation analysis data
#' fit <- rls_fit(model = model, data = D)
#' # What did we get back?
#' names(fit)
#' # The one-step forecast
#' plot(D$y, type="l")
#' lines(fit$Yhat$k1, col=2)
#' # The one-step RLS coefficients over time (Lfitval is a list of the fits for each horizon)
#' plot(fit$Lfitval$k1$Ta, type="l")
#'
#' # A summary
#' summary(fit)
#' # Plot the fit
#' plot_ts(fit, kseq=1)
#'
#' # Fitting with lower lambda makes the RLS coefficients change faster
#' fit2 <- rls_fit(prm = c(lambda=0.9), model, D)
#' plot_ts(fit2, kseq=1)
#'
#'
#' # It can return a score
#' rls_fit(c(lambda=0.9), model, D, scorefun=rmse, returnanalysis=FALSE)
#'
#' # Such that it can be passed to an optimzer (see ?rls_optim for a nice wrapper of optim)
#' val <- optim(c(lambda=0.99), rls_fit, model = model, data = D, scorefun = rmse,
#'              returnanalysis=FALSE)
#' val$par
#' # Which can then simply be applied
#' rls_fit(val$par, model, D, scorefun=rmse, returnanalysis=FALSE)
#' # see ?rls_optim, how optim is wrapped for a little easiere use
#'
#' # See rmse as a function of horizon
#' fit <- rls_fit(val$par, model, D, scorefun = rmse)
#' plot(fit$scoreval, xlab="Horizon k", ylab="RMSE")
#' # See ?score_fit for a little more consistent way of calculating this
#'
#'
#' # Try adding a low-pass filter to Ta
#' model$add_inputs(Ta = "lp(Ta, a1=0.92)")
#' # To obtain the transformed data, i.e. the data which is used as input to the RLS
#' model$reset_state()
#' # Generate the the transformed data
#' datatr <- model$transform_data(D)
#' # What did we get?
#' str(datatr)
#' # See the effect of low-pass filtering
#' plot(D$Ta$k1, type="l")
#' lines(datatr$Ta$k1, col=2)
#' # Try changing the 'a1' coefficient and rerun
#' # ?rls_optim for how to optimize also this coefficient
#'
#' 
#' @export

rls_fit <- function(prm=NA, model, data, scorefun = NA, returnanalysis = TRUE,
                    runcpp = TRUE, printout = TRUE){
    # Check that the model is setup correctly, it will stop and print a message if not
    model$check(data)
    
    # Function for initializing an rls fit:
    # - it will change the "model" input (since it an R6 class and thus passed by reference
    # - If scorefun is given, e.g. rmse() then the value of this is returned
    #

    if(printout){
        # Should here actually only print the ones that were found and changed?
        message("----------------")
        if(is.na(prm[1])){
            message("prm=NA, so current parameters are used.")
        }else{
            print_to_message(prm)
        }
    }

    # First insert the prm into the model input expressions
    model$insert_prm(prm)

    # Since rls_fit is run from scratch, the init the stored inputs data (only needed when running iteratively)
    model$datatr <- NA
    model$yAR <- NA

    # Reset the model state (e.g. inputs state, stored iterative data, ...)
    model$reset_state()
    # Generate the 2nd stage inputs (i.e. the transformed data)
    datatr <- model$transform_data(data)

    # Initialize the fit for each horizon
    # Need to know how many inputs to be fitted with?
    np <- length(datatr)

    #
    model$Lfits <- lapply(model$kseq, function(k){
        fit <- list(k = k,
                    # Init values for the parameter vector
                    theta = matrix(rep(0,np), ncol = 1))
        if(runcpp){
            # cpp rls version use covariance P
            fit$P <- diag(10000,np)
        }else{
            # rls version use inverse covariance R
            fit$R <- diag(1/10000,np)
        }
        #
        return(fit)
    })
    names(model$Lfits) <- pst("k", model$kseq)

    # Calculate the parameter estimates for each time point
    Lresult <- rls_update(model, datatr, data[[model$output]], runcpp)
    Yhat <- lapply_cbind_df(Lresult, function(x){
        x$yhat
    })
    nams(Yhat) <- pst("k",model$kseq)

    # Maybe crop the output
    if(!is.na(model$outputrange[1])){ Yhat[Yhat < model$outputrange[1]] <- model$outputrange[1] }
    if(!is.na(model$outputrange[2])){ Yhat[Yhat > model$outputrange[2]] <- model$outputrange[2] }

    #----------------------------------------------------------------
    # Calculate the result to return
    # If the objective function (scorefun) is given
    if(class(scorefun) == "function"){
        # Do some checks
        if( !("scoreperiod" %in% names(data)) ){ stop("data$scoreperiod is not set: Must have it set to an index (int or logical) defining which points to be evaluated in the scorefun().") }
        if( all(is.na(data$scoreperiod)) ){ stop("data$scoreperiod is not set correctly: It must be set to an index (int or logical) defining which points to be evaluated in the scorefun().") }
        # Calculate the objective function for each horizon
        Residuals <- residuals(Yhat, data[[model$output]])
        scoreval <- sapply(1:ncol(Yhat), function(i){
            scorefun(Residuals[data$scoreperiod,i])
        })
        nams(scoreval) <- nams(Yhat)
    }else{
        scoreval <- NA
    }

    # 
    if(returnanalysis){
        # The estimated coefficients
        Lfitval <- getse(Lresult, "Theta", fun=as.data.frame)
        # Return the model validation data
        invisible(structure(list(Yhat = Yhat, model = model$clone_deep(), data = data, datatr = datatr, Lfitval = Lfitval, scoreval = scoreval), class = c("forecastmodel_fit","rls_fit")))
    }else{
        # Only the summed score returned
        val <- sum(scoreval, na.rm = TRUE)
        if(is.na(val)){ stop("Cannot calculate the scorefunction for any horizon") }
        if(printout){
            print_to_message(c(scoreval,sum=val))
        }
        return(val)
    }
}
