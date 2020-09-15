#' Use a fitted forecast model to predict its output variable with transformed data.
#'
#' See the ??ref(recursive updating vignette, not yet available).
#'
#' @title Prediction with an lm forecast model.
#' @param model Onlineforecast model object which has been fitted.
#' @param datatr Transformed data.
#' @return The Yhat forecast matrix with a forecast for each model$kseq and for each time point in \code{datatr$t}.
#' @examples
#'
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a model 
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "lp(Ta, a1=0.7)", mu = "one()")
#'
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#'
#' # Transform using the mdoel
#' datatr <- model$transform_data(D)
#'
#' # See the transformed data
#' str(datatr)
#'
#' # The model has not been fitted
#' model$Lfits
#'
#' # To fit
#' lm_fit(model=model, data=D)
#'
#' # Now the fits for each horizon are there (the latest update)
#' # For example 
#' summary(model$Lfits$k1)
#'
#' # Use the fit for prediction
#' D$Yhat <- lm_predict(model, datatr)
#'
#' # Plot it
#' plot_ts(D, c("y|Yhat"), kseq=1)
#'
#' @importFrom stats predict
#' @export
lm_predict <- function(model, datatr) {
  # Calculate the predictions
  Yhat <- lapply_cbind_df(1:length(model$kseq), function(i){
    k <- model$kseq[i]
    fit <- model$Lfits[[i]]
    # Form the regressor matrix, don't lag
    X <- as.data.frame(subset(datatr, kseq = k))
    predict(fit, X)
  })
  nams(Yhat) <- pst("k", model$kseq)
  return(Yhat)
}
