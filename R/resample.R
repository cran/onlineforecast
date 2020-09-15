# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?resample
#?resample.data.frame

#' Make a downsampling to a lower sampling frequency
#'
#' Given an object with a column indicating the time points of the observations the
#' function returns a similar object, where the function is applied for each new (and longer)
#' interval.
#'
#' Typically it is used if for example 15 minute values should be made into 1 hour values.
#'
#' NOTE that it is always assumed that the time point is at the end of the time interval,
#' e.g. if hourly values are returned, then "2019-01-01 01:00" indicates the first hour in 2019.
#'
#' All time points at the time point (border) of between two intervals is assigned to the
#' first interval of the two.
#' 
#' @title Resampling to equidistant time series
#' @param object Can be data.frame
#' @param ts (numeric) New sample period in seconds
#' @param tstart A POSIXxx (or charater or numeric), which indicates the first time point in the series returned
#' @param tend A POSIXxx (or charater or numeric), which indicates the last time point in the series returned
#' @param timename (character) The name of the time column in object
#' @param fun (function) The function of apply. Default is mean, such that average values are obtained
#' @param quantizetime (logical) Should the new time points be set to the end of the time intervals, or should they also be the result of the fun function
#' @param ... Passed on to the fun function
#' @return A downsampled data.frame
#' @examples
#'
#' # Generate some test data with 10 minutes sampling frequency for one day
#' X <- data.frame(t=seq(ct("2019-01-01 00:10"),ct("2019-01-02"), by=10*60))
#' 
#' # A single sine over the day
#' X$val <- sin(as.numeric(X$t)/3600*2*pi/(24))
#'
#' # Resample to hourly average values
#' Xre <- resample(X, 3600)
#' plot(X$t, X$val)
#' lines(Xre$t, Xre$val, type="b", col=2)
#'
#' # Resample to hourly max values
#' Xre <- resample(X, 3600, fun=max)
#' lines(Xre$t, Xre$val, type="b", col=3)
#'
#' # Another starting time point
#' Xre <- resample(X, 3600, tstart="2019-01-01 00:30")
#' lines(Xre$t, Xre$val, type="b", col=4)
#'
#' 
#' @export
resample <- function(object, ts, tstart=NA, tend=NA, timename="t", fun=mean, quantizetime=TRUE, ...){
    UseMethod("resample")
}

#' @importFrom stats aggregate
#' @export
resample.data.frame <- function(object, ts, tstart=NA, tend=NA, timename="t", fun=mean, quantizetime=TRUE, ...)
{
    X <- object
    # ----------------------------------------------------------------
    # Do nothing if ts is NA
    if(is.na(ts)){ return(X) }
    
    # ----------------------------------------------------------------
    # If no start time. The start time will be set to the first value, floored with ts
    if(is.na(tstart)){ tstart <- X[1,timename] - as.numeric(X[1,timename],"secs") %% ts }
    # If no end time is given then set it
    if(is.na(tend)){ tend <- X[nrow(X),timename]}

    # ----------------------------------------------------------------
    # Convert to POSIXct
    tstart <- ct(tstart)
    tend <- ct(tend)
    
    # ----------------------------------------------------------------
    # Cut out the time period
    X <- X[tstart<X[,timename] & X[,timename]<=tend,]
    # Remove values with a NA value in time
    X <- X[!is.na(X[,timename]), ]
    
    # ----------------------------------------------------------------
    # Split into periods of length ts, and take the fun function of each period
    X[ ,timename] <- (as.numeric(X[ ,timename], units="secs") - as.numeric(tstart, units="secs"))
    iSplit <- -(X[ ,timename] %/% -ts)
    # Do the resampling
    Xres <- aggregate(X, list(iSplit), fun, ...)
    # Remove the "Group" column
    Xres <- Xres[,-1]
    # Convert time to POSIXct
    Xres[ ,timename] <- tstart + Xres[ ,timename]

    # Include intervals with NA in the result
    Xres <- cbind(Xres,iSplit=unique(iSplit))
    iSplit <- 1:-((as.numeric(tend, units="secs")-as.numeric(tstart, units="secs")) %/% -ts)
    withNA <- data.frame(iSplit=iSplit)
    Xres <- merge(Xres,withNA,all=TRUE)
    # Remove the iSplit column
    Xres <- Xres[,-match("iSplit",names(Xres))]
    if(quantizetime)
    {
        # Set the time points to the end of each interval
        time <- seq(tstart,by=ts,length.out=nrow(Xres)) + ts
        Xres[,timename] <- time
    }
    
    return(Xres)
}
