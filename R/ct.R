# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?ct
#?ct.default

#' The object is converted into POSIXct with tz="GMT".
#'
#' A simple helper, which wraps \code{\link{as.POSIXct}}` and sets the time zone to "GMT" per default.
#' 
#' @title Convertion to POSIXct
#' @param object The object to convert can be: character, numeric, POSIXct or POSIXlt
#' @param tz Timezone. If set, then the time zone will be changed of the object.
#' @param ... Arguments to be passed to methods.
#' @return An object of class POSIXct
#' @section Methods:
#' @examples
#'
#'
#' # Create a POSIXct with tz="GMT"
#' ct("2019-01-01")
#' class(ct("2019-01-01"))
#' ct("2019-01-01 01:00:05")
#'
#'
#' # Convert to POSIXct
#' class(ct(as.POSIXlt("2019-01-01")))
#' 
#' # To seconds and back again
#' ct(as.numeric(1000, units="sec"))
#'
#'
#' # --------
#' # Convert character of time which has summer time leaps
#' # Example from CET (with CEST which is winter time)
#' # 
#' # The point of shifting to and from summer time:
#' # DST Start (Clock Forward)	DST End (Clock Backward)
#' # Sunday, March 31, 02:00	Sunday, October 27, 03:00
#'
#' # --------
#' # From to winter time to summer time
#' txt <- c("2019-03-31 01:00",
#'          "2019-03-31 01:30",
#'          "2019-03-31 03:00",
#'          "2019-03-31 03:30")
#' x <- ct(txt, tz="CET")
#' x
#' ct(x, tz="GMT")
#'
#' # BE AWARE of this conversion of the 02:00: to 02:59:59 (exact time of shift) will lead to a
#' # wrong conversion
#' txt <- c("2019-03-31 01:30",
#'          "2019-03-31 02:00",
#'          "2019-03-31 03:30")
#' x <- ct(txt, tz="CET")
#' x
#' ct(x, tz="GMT")
#' # Which a diff on the time can detect, since all steps are not equal
#' plot(diff(ct(x, tz="GMT")))
#' 
#' # --------
#' # Shift to winter time is more problematic
#' # It works like this 
#' txt <- c("2019-10-27 01:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 03:00",
#'          "2019-10-27 03:30")
#' x <- ct(txt, tz="CET")
#' x
#' ct(x, tz="GMT")
#'
#' # however, timestamps can be given like this
#' txt <- c("2019-10-27 01:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 03:00",
#'          "2019-10-27 03:30")
#' x <- ct(txt, tz="CET")
#' x
#' ct(x, tz="GMT")
#' # Again can be detected, since all steps are not equal
#' plot(diff(ct(x, tz="GMT")))
#' # This can be fixed by (note that it can go wrong, e.g. with gaps around convertion etc.)
#' ct(x, tz="GMT", duplicatedadd=3600)
#'
#' @export

ct <- function(object, ...){
    UseMethod("ct")
}


#' @rdname ct
#' @section Methods:
#'     - ct.character: Simply a wrapper for \code{as.POSIXct} with default \code{tz}
#' @export
ct.character <- function(object, tz = "GMT", ...){
    as.POSIXct(object, tz=tz, ...)
}

#' @rdname ct
#' @param duplicatedadd Seconds to be added to duplicated time stamps, to mitigate the problem of duplicated timestamps at the shift to winter time. So the second time a time stamp occurs (identified with \code{duplicated}) then the seconds will be added.
#' @section Methods:
#'     - ct.POSIXct: Changes the time zone of the object if \code{tz} is given.
#' @export
ct.POSIXct <- function(object, tz = NA, duplicatedadd = NA, ...){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    if(!is.na(duplicatedadd)){
        # To mitigate the problem of duplicated timestamps at the shift to winter time
        # then shift the time of duplicated time stamps with the seconds given
        object[which(duplicated(object))] <- object[which(duplicated(object))] + duplicatedadd
    }
    return(object)
}

#' @rdname ct
#' @section Methods:
#'     - ct.POSIXlt: Converts to POSIXct.
#' @export
ct.POSIXlt <- function(object, tz = NA, duplicatedadd = NA, ...){
    as.POSIXct(ct.POSIXct(object, tz, duplicatedadd), ...)
}

#' @rdname ct
#' @section Methods:
#'     - ct.numeric: Converts from UNIX time in seconds to POSIXct with \code{tz} as GMT.
#' @export
ct.numeric <- function(object, ...){
    ISOdate(1970, 1, 1, 0, ...) + object
}
