# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lm_fit

#' Fit a linear regression model given a onlineforecast model, seperately for each prediction horizon
#'
#' @title Fit an onlineforecast model with \code{\link{lm}}
#' @param prm as numeric with the parameters to be used when fitting.
#' @param model object of class forecastmodel with the model to be fitted.
#' @param data as data.list with the data to fit the model on.
#' @param scorefun Optional. If scorefun is given, e.g. \code{\link{rmse}}, then the value of this is also returned.
#' @param returnanalysis as logical determining if the analysis should be returned. See below.
#' @param printout Defaults to TRUE. Prints the parameters for model.
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
#'         * \code{Lfitval}: a character "Find the fits in model$Lfits", it's a list with the lm fits for each horizon.
#'
#'         * \code{scoreval}: data.frame with the scorefun result on each horizon (only scoreperiod is included).
#'
#'     - If \code{returnanalysis} is FALSE (and \code{scorefun} is given): The sum of the score function on all horizons (specified with model$kseq).
#'
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$output <- "y"
#' model$add_inputs(Ta = "Ta",
#'                  mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#'
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#'
#' # Now we can fit the model with RLS and get the model validation analysis data
#' fit <- lm_fit(prm=NA, model=model, data=D)
#' # What did we get back?
#' names(fit)
#' class(fit)
#' # The one-step forecast
#' plot(D$y, type="l")
#' lines(fit$Yhat$k1, col=2)
#' # Get the residuals
#' plot(residuals(fit)$h1)
#' # Score for each horizon
#' score_fit(fit)
#'
#' # The lm_fit don't put anything in
#' fit$Lfitval
#' # Find the lm fits here
#' model$Lfits
#' # See result for k=1 horizon
#' summary(model$Lfits$k1)
#' # Some diurnal pattern is present
#' acf(residuals(fit)$h1, na.action=na.pass, lag.max=96)
#'
#' @importFrom stats lm residuals
#' @export
lm_fit <- function(prm=NA, model, data, scorefun = NA, returnanalysis = TRUE, printout = TRUE){
    # Check that the model is setup correctly, it will stop and print a message if not
    model$check(data)
    
    # Function for initializing an lm fit:
    # - it will change the "model" input (since it an R6 class and thus passed by reference
    # - If scorefun is given, e.g. rmse() then the value of this is returned

    if(printout){
        # Should here actually only print the one that were found and changed?
        message("----------------")
        if(is.na(prm[1])){
            message("prm=NA, so current parameters are used.")
        }else{
            print_to_message(prm)
        }
    }
    # First insert the prm into the model input expressions
    model$insert_prm(prm)

    # ################################
    # Since lm_fit is run from scratch, the init the stored inputs data (only needed when running iteratively)
    model$datatr <- NA
    model$yAR <- NA
    
    # ################################ 
    # Init the inputs states (and some more is reset)
    model$reset_state()
    # Generate the 2nd stage inputs (i.e. the transformed data)
    datatr <- model$transform_data(data)

    #
    model$Lfits <- lapply(model$kseq, function(k){
      # Form the regressor matrix, and lag
      X <- as.data.frame(subset(datatr, kseq = k, lagforecasts = TRUE))
      inputnms <- names(X)
      # Add the model output to the data.frame for lm()
      X[ ,model$output] <- data[[model$output]]
      # Generate the formula
      frml <- pst(model$output, " ~ ", pst(inputnms, collapse=" + "), " - 1")
      # Fit the model
      fit <- lm(frml, X)
      # Return the fit and the data
      return(fit)
    })
    names(model$Lfits) <- pst("k", model$kseq)

    # Calculate the predictions
    Yhat <- lm_predict(model, datatr)

    # Maybe crop the output
    if(!is.na(model$outputrange[1])){ Yhat[Yhat < model$outputrange[1]] <- model$outputrange[1] }
    if(!is.na(model$outputrange[2])){ Yhat[model$outputrange[1] < Yhat] <- model$outputrange[2] }

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
        # Return the model validation data
        invisible(structure(list(Yhat = Yhat, model = model$clone_deep(), data = data, Lfitval = "Find the lm fits in model$Lfits", scoreval = scoreval), class = c("forecastmodel_fit","lm_fit")))
    }else{
        # Only the summed score returned
        val <- sum(scoreval, na.rm = TRUE)
        if(is.na(val)){ stop("Cannot calculate the scorefunction for any horizon") }
        if(printout){ print_to_message(c(scoreval,sum=val))}
        return(val)
    }

    ## OLD
    ## # Is an objective function given?
    ## if(class(scorefun) == "function" & !returnanalysis){
    ##     # Do some checks
    ##     if( !("scoreperiod" %in% names(data)) ){ stop("data$scoreperiod are set: Must have it set to an index (int or logical) defining which points to be evaluated in the scorefun().") }
    ##     if( all(is.na(data$scoreperiod)) ){ stop("data$scoreperiod is not set correctly: It must be set to an index (int or logical) defining which points to be evaluated in the scorefun().") }
    ##     scoreperiod <- data$scoreperiod    
    ##     # Return the scorefun values
    ##     scoreval <- sapply(1:ncol(Yhat), function(i){
    ##         scorefun(Resid[scoreperiod,i])
    ##     })
    ##     nams(scoreval) <- nams(Yhat)
    ##     val <- sum(scoreval, na.rm = TRUE)
    ##     if(printout){print(c(scoreval,sum=val))}
    ##     return(val)
    ## } else if(returnanalysis){
    ##     # The estimated coefficients
    ##     Lfitval <- lapply(model$Lfits, function(model){ 
    ##       coef <- model$coefficients
    ##       names(coef) <- gsub("(.+?)(\\.k.*)", "\\1", names(coef))
    ##       return(coef)
    ##     })
    ##     # Include score function
    ##     scoreval <- NA
    ##     if(class(scorefun) == "function"){
    ##         # Calculate the objective function for each horizon
    ##         scoreval <- sapply(1:ncol(Yhat), function(i){
    ##             scorefun(Resid[,i])
    ##         })
    ##         nams(scoreval) <- nams(Yhat)
    ##     }
    ##     # Return the model validation data
    ##     return(list(Yhat = Yhat, t = data$t, Resid = Resid, datatr = datatr, Lfitval = Lfitval, scoreval = scoreval, scoreperiod = data$scoreperiod))
    ## }
    ## invisible("ok")
}
