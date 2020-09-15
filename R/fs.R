#' Generation of Fourrier series.
#'
#' Function for generating Fourrier series as a function of x E.g. use for
#' harmonic functions for modelling the diurnal patterns or for basis functions.
#'
#' @param X must be a dataframe with columns k1,k2,..., . One period is from 0 to 1
#' (so for example if X is hour of day, then divide X by 24 to obtain a daily period).
#' @param nharmonics the number of harmonics, so creates double as many inputs! i.e. one sine and one cos for each harmonic.
#' @return  Returns a list of dataframes (two for each i in \code{1:nharmonics}) with same number of columns as X.
#' @examples
#' # Make a data.frame with time of day in hours for different horizons
#' tday <- make_tday(seq(ct("2019-01-01"), ct("2019-01-04"), by=3600), kseq=1:5)
#' # See whats in it
#' str(tday)
#' head(tday)
#'
#' # Now use the function to generate Fourier series
#' L <- fs(tday/24, nharmonics=2)
#' # See what is in it
#' str(L)
#'
#' # Make a plot to see the harmonics
#' par(mfrow=c(2,1))
#' # The first harmonic
#' plot(L$sin1$k1, type="l")
#' lines(L$cos1$k1, type="l")
#' # The second harmonic
#' plot(L$sin2$k1, type="l")
#' lines(L$cos2$k1, type="l")
#'
#' 
#' @export
fs <- function(X, nharmonics) {
    do.call("c", lapply(1:nharmonics, function(i) {
        val <- list(sin(i * X * 2 * pi), cos(i * X * 2 * pi))
        nams(val) <- pst(c("sin", "cos"), i)
        return(val)
    }))
}
