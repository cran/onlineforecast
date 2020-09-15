# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?aslt
#?aslt.default

#' The argument is converted into POSIXlt with tz="GMT".
#'
#' 
#' 
#' @title Convertion to POSIXlt
#' @param object The character, POSIXct, POSIClt, or numeric which is converted to POSIXct.
#' @param tz Timezone. If set, then the time zone will be changed of the object.
#' @param ... Arguments to be passed to methods.
#' @return An object of class POSIXlt
#' @section Methods:
#' #' @examples
#' 
#' # Create a POSIXlt with tz="GMT"
#' aslt("2019-01-01")
#' class(aslt("2019-01-01"))
#' aslt("2019-01-01 01:00:05")
#'
#' # Convert between time zones
#' x <- aslt("2019-01-01", tz="CET")
#' aslt(x,tz="GMT")
#'
#' # To seconds and back again
#' aslt(as.numeric(x, units="sec"))
#' 
#' @export
aslt <- function(object, ...){
    UseMethod("aslt")
}

#' @rdname aslt
#' @section Methods:
#'     - aslt.character: Simply a wrapper for \code{as.POSIXlt}
#' @export
aslt.character <- function(object, tz = "GMT", ...){
    as.POSIXlt(object, tz = tz, ...)
}

#' @rdname aslt
#' @section Methods:
#'     - aslt.POSIXct: Converts to POSIXct.
#' @export
aslt.POSIXct <- function(object, tz = NA, ...){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    as.POSIXlt(object, ...)
}

#' @rdname aslt
#' @section Methods:
#'     - aslt.POSIXlt: Changes the time zone of the object if tz is given.
#' @export
aslt.POSIXlt <- function(object, tz = NA, ...){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    return(object)
}

#' @rdname aslt
#' @section Methods:
#'     - aslt.numeric: Converts from UNIX time in seconds to POSIXlt.
#' @export
aslt.numeric <- function(object, ...){
    as.POSIXlt(ISOdate(1970, 1, 1, 0, ...) + object)
}
