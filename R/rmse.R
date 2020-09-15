# Do this in a separate file to see the generated help:
# library(devtools)
# document()
# load_all(as.package("../../onlineforecast"))
# ?rmse

#' Returns the RMSE.
#'
#' Used for forecast evaluation evaluation and optimization of parameters in model fitting.
#'
#' Note that \code{NA}s are ignored (i.e. \code{mean} is called with \code{na.rm=TRUE}).
#'
#' @title Computes the RMSE score.
#' @param x a numerical vector of residuals.
#' @return The RMSE score.
#' @seealso \code{\link{score}()} for calculation of a score for the k'th horizon, and \code{\link{score_fit}()} which takes a forecastmodel fit and returns score taking scoreperiod etc. into account.
#' @name rmse
#' @examples
#'
#'
#'  # Just a vector to be forecasted
#'  y <- c(filter(rnorm(100), 0.95, "recursive"))
#'  # Generate a forecast matrix with a simple persistence model
#'  Yhat <- persistence(y, kseq=1:4)
#'  # The residuals for each horizon
#'  Resid <- residuals(Yhat, y)
#'
#' # Calculate the score for the k1 horizon
#' rmse(Resid$h1)
#'
#' # For all horizons
#' apply(Resid, 2, rmse)
#'
#' 
#' @export

rmse <- function(x) {
    sqrt(mean(x^2, na.rm = TRUE))
}
