## Do this in a separate tmp.R file to check the documentation
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?make_tday

#' Make an hour-of-day data.frame with k-step ahead columns.
#'
#' This function creates a data.frame with k-steps-ahead values of hour of day,
#' such that it can be added to a data.list and used inputs to a forecast model.
#' 
#' @param time vector of times of class "POSIXct" "POSIXt".
#' @param kseq vector of integers, respresenting the desired "k-steps ahead".
#' @param tstep step time of k in seconds.
#' @param units to return in, e.g. "hours" or "mins"
#' @return Returns a data.frame with rownames = times, colnames = k1, k2, k5, ...
#' The content of the data frame is the hour of day, following the setup in "onlineforecast" setup.
#' @keywords hourofday lags data.frame
#' @examples
#' # Create a time sequence
#' tseq <- seq(ct("2019-01-01"), ct("2019-02-01 12:00"), by=1800)
#' 
#' # Make the time of day sequence
#' make_tday(tseq, 1:10)
#' 
#' # With 0.5 hour steps and in minutes
#' make_tday(tseq, 1:10, tstep=1800, units="mins")
#'
#' 
#' @export

make_tday <- function(time, kseq, tstep=3600, units="hours"){
    ## The time of day (in the specified units)
    tday <- sapply(kseq, function(k){
        tk <- time + k * tstep
        as.numeric( tk - trunc(tk, units="days"), units=units)
    })
    ## set row and column names
    nams(tday) <- paste0('k', kseq)
    return( as.data.frame(tday) )
}
