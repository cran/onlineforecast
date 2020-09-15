## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?long_format

#' Creates a long format of the predictions
#'
#' This functions creates a useful prediction data.frame which can be useful for analysis and plotting.
#'
#' @title Long format of prediction data.frame
#' @param fit The result from either lm_fit or rls_fit
#' @param Time If the timestamps are missing from the fit object
#' @return Data.frame of when the prediction where made, also the prediction value and timestamp.
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # Define a model
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "Ta",
#'                  mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#' model$kseq <- 1:6
#' # Fit it
#' fit <- rls_fit(prm=c(lambda=0.99), model, D)
#'
#' # Get the forecasts (in fit$Yhat) on long format
#' long_format(fit)
#' 
#' @export

long_format <- function(fit, Time = NULL){
    if(!("t" %in% names(fit$data))) {
        if(is.null(Time)) stop("Missing Time (either fit$data$t or Time)")
        fit$data$t <- Time
    }
    if(!("Yhat" %in% names(fit))) stop("Missing forecasts (Yhat in fit)")
    #
    predDF <- do.call(rbind, lapply(1:length(fit$data$t), function(i)
    {
        DF <- data.frame(PredTime = fit$data$t[i],
                   Time = fit$data$t[(i+1):(dim(fit$Yhat)[2]+i)],
                   k = 1:(dim(fit$Yhat)[2]),
                   Pred = as.numeric(fit$Yhat[i,]))
        return(DF)
    }))
    return(predDF)
}
