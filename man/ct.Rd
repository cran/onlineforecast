% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct.R
\name{ct}
\alias{ct}
\alias{ct.character}
\alias{ct.POSIXct}
\alias{ct.POSIXlt}
\alias{ct.numeric}
\title{Convertion to POSIXct}
\usage{
ct(object, ...)

\method{ct}{character}(object, tz = "GMT", ...)

\method{ct}{POSIXct}(object, tz = NA, duplicatedadd = NA, ...)

\method{ct}{POSIXlt}(object, tz = NA, duplicatedadd = NA, ...)

\method{ct}{numeric}(object, ...)
}
\arguments{
\item{object}{The object to convert can be: character, numeric, POSIXct or POSIXlt}

\item{...}{Arguments to be passed to methods.}

\item{tz}{Timezone. If set, then the time zone will be changed of the object.}

\item{duplicatedadd}{Seconds to be added to duplicated time stamps, to mitigate the problem of duplicated timestamps at the shift to winter time. So the second time a time stamp occurs (identified with \code{duplicated}) then the seconds will be added.}
}
\value{
An object of class POSIXct
}
\description{
The object is converted into POSIXct with tz="GMT".
}
\details{
A simple helper, which wraps \code{\link{as.POSIXct}}` and sets the time zone to "GMT" per default.
}
\section{Methods}{



    - ct.character: Simply a wrapper for \code{as.POSIXct} with default \code{tz}


    - ct.POSIXct: Changes the time zone of the object if \code{tz} is given.


    - ct.POSIXlt: Converts to POSIXct.


    - ct.numeric: Converts from UNIX time in seconds to POSIXct with \code{tz} as GMT.
}

\examples{


# Create a POSIXct with tz="GMT"
ct("2019-01-01")
class(ct("2019-01-01"))
ct("2019-01-01 01:00:05")


# Convert to POSIXct
class(ct(as.POSIXlt("2019-01-01")))

# To seconds and back again
ct(as.numeric(1000, units="sec"))


# --------
# Convert character of time which has summer time leaps
# Example from CET (with CEST which is winter time)
# 
# The point of shifting to and from summer time:
# DST Start (Clock Forward)	DST End (Clock Backward)
# Sunday, March 31, 02:00	Sunday, October 27, 03:00

# --------
# From to winter time to summer time
txt <- c("2019-03-31 01:00",
         "2019-03-31 01:30",
         "2019-03-31 03:00",
         "2019-03-31 03:30")
x <- ct(txt, tz="CET")
x
ct(x, tz="GMT")

# BE AWARE of this conversion of the 02:00: to 02:59:59 (exact time of shift) will lead to a
# wrong conversion
txt <- c("2019-03-31 01:30",
         "2019-03-31 02:00",
         "2019-03-31 03:30")
x <- ct(txt, tz="CET")
x
ct(x, tz="GMT")
# Which a diff on the time can detect, since all steps are not equal
plot(diff(ct(x, tz="GMT")))

# --------
# Shift to winter time is more problematic
# It works like this 
txt <- c("2019-10-27 01:30",
         "2019-10-27 02:00",
         "2019-10-27 02:30",
         "2019-10-27 03:00",
         "2019-10-27 03:30")
x <- ct(txt, tz="CET")
x
ct(x, tz="GMT")

# however, timestamps can be given like this
txt <- c("2019-10-27 01:30",
         "2019-10-27 02:00",
         "2019-10-27 02:30",
         "2019-10-27 02:00",
         "2019-10-27 02:30",
         "2019-10-27 03:00",
         "2019-10-27 03:30")
x <- ct(txt, tz="CET")
x
ct(x, tz="GMT")
# Again can be detected, since all steps are not equal
plot(diff(ct(x, tz="GMT")))
# This can be fixed by (note that it can go wrong, e.g. with gaps around convertion etc.)
ct(x, tz="GMT", duplicatedadd=3600)

}
