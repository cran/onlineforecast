
#' Calculate the score for each horizon for a forecast model fit.
#'
#' For evaluation of the score on each horizon, as specified in the fit. Use it for a consistent evaluation.
#' 
#' @title Calculates scores for a forecast model fit.
#' @param fit A model fit 
#' @param scoreperiod as an index (logical or integer) defining which points to inlude in the score calculation. If NA, the \code{scoreperiod} from the \code{fit$model} object is used.
#' @param usecomplete Only use points where 
#' @param scorefun The score function applied, per default \code{\link{rmse}}.
#' @seealso \code{\link{rmse}} and \code{\link{score}} which are used in this function.
#' @return A list with:
#'   - \code{scoreval} is the score value
#'   - \code{scoreperiod} is the score period used (can be different that the one in arguments \code{fit} or \code{scoreperiod})
#'   - \code{scorename} is the name of the score function applied
#' @export
score_fit <- function(fit, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse){
    # Calculate the score for each horizon
    if("scorefun" %in% names(as.list(match.call()))){
        scorename <- as.list(match.call())$scorefun
    }else{
        scorename = "rmse"
    }

    # Check score period
    txt <- "It must be set to an index (int or logical) defining which points to be evaluated in the scorefun()."
    if(is.na(scoreperiod[1])){
        if("scoreperiod" %in% nams(fit$data)){
            scoreperiod <- fit$data$scoreperiod
        }else{
            stop("scoreperiod is not set. Set it in the data used in the fit function or as argument in the present call: ",txt)
        }
    }

    # Calculate the Residuals if they were not in fit
    if( !"Residuals" %in% names(fit) ){
        # Calculate the residuals
        Residuals <- residuals(fit$Yhat, fit$data[fit$model$output])
    }else{
        Residuals <- fit$Residuals
    }

    # Calculate the score
    tmp <- score(Residuals, scoreperiod, usecomplete)
    scoreval <- tmp$scoreval
    scoreperiod <- tmp$scoreperiod
    
    # Give a warning if the score 
    if(!is.na(fit$scoreval[1])){
        if(length(fit$scoreval) != length(scoreval)){
            warning("fit contains 'scoreval', which is different in length than the scoreval calculated here (probably for different horizons (kseq))")
        }else if(!all(fit$scoreval == scoreval)){
            warning("fit contains 'scoreval' which is different from the",scorename,"score calculated now (can also be because of 'usecomplete = TRUE', such that only points which have forecasts for all horizons are included.")
        }
    }
    #
    list(scoreval=scoreval, scoreperiod=scoreperiod, scorename=scorename)
}
