# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?in_range

#' Returns a logical vector of boolean values where TRUE indicates if timestamp is within the
#' specified period.
#'
#' Returns a logical vector of boolean values where TRUE indicates if timestamp is within the
#' specified period spanned by tstart and tend. 
#'
#' Note the convention of time stamp in the end of the time intervals causes the time point
#' which equals \code{tstart} not to be included. See last example.
#'
#' The times can be given as character or POSIX, per default in tz='GMT'.
#' 
#' @title Selects a period
#' @param tstart The start of the period.
#' @param time The timestamps as POSIX.
#' @param tend The end of the period. If not given then the period will have no end. 
#' @return A logical vector indicating the selected period with TRUE
#' @name in_range
#' @examples
#'
#' # Take a subset
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#'
#' # Just a logical returning TRUE in a specified period
#' in_range("2010-12-20", D$t, "2010-12-22")
#'
#' # Set which period to evaluate when optimizing parameters, like in rls_optim()
#' # (the points with scoreperiod == false are not included in the score evaluation)
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' D$scoreperiod
#'
#' # Further, excluding a small period by
#' D$scoreperiod[in_range("2010-12-26", D$t, "2010-12-27")] <- FALSE
#' D$scoreperiod
#'
#' # Note the convention of time stamp in the end of the time intervals
#' # causes the point with t = 2010-12-26 00:00:00 not to be included
#' # since it's covering to "2010-12-25 23:00:00" to "2010-12-26 00:00:00"
#' D$t[in_range("2010-12-26", D$t, "2010-12-27")]
#'
#'
#' @export

in_range <- function(tstart, time, tend=NA) {
    if (class(tstart)[1] == "character") 
        tstart <- ct(tstart)
    if (is.na(tend))
        tend <- time[length(time)]
    if (class(tend)[1] == "character") 
        tend <- ct(tend)
    ct(tstart) < time & time <= ct(tend)
}
