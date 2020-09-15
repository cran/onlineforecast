# Do this in a separate file to see the generated help:
# library(devtools)
# document()
# load_all(as.package("../../onlineforecast"))
# ?as.data.list
# ?as.data.list.data.frame


#' These functions will convert the object into a data.list.
#'
#' A data.list is simply a list of vectors and data.frames. For the use in the 
#' onlineforecast package the following format must be kept:
#' 
#'   - t: A vector of time.
#' 
#'   - vectors with same length as t: Holds observations and values synced to time t.
#' 
#'   - data.frames with number of rows as time t: Holds forecasts in each column named by \code{kxx} where \code{xx} is the
#'                                                horizon, e.g. \code{k0} is synced as observations, and \code{k1} is one-step ahead.
#'
#' @title Convert to data.list class
#' @param object The object to be converted into a data.list
#' @return a value of class data.list
#' @seealso \code{For specific detailed info see the children, e.g. \link{as.data.list.data.frame} }
#' @family as.data.list
#' 
#' @export
as.data.list <- function(object){
    UseMethod("as.data.list")
}




#' Convert a data.frame into a data.list
#'
#' The convention is that columns with forecasts are postfixed with \code{.kxx} where
#' \code{xx} is the horizon. See the examples.
#'
#' @title Convertion of data.frame into data.list
#' @param object The data.frame to be converted.
#' @return a data.list
#' @seealso as.data.list
#' @family as.data.list
#' @examples
#' # Convert a dataframe with time and two observed variables
#' X <- data.frame(t=1:10, x=1:10, y=1:10)
#' as.data.list(X)
#'
#' # Convert a dataframe with time, forecast and an observed variable
#' X <- data.frame(t=1:10, x.k1=1:10, x.k2=10:1, yobs=1:10, y.k1=1:10, y.k2=1:10)
#' as.data.list(X)
#'
#' # Can be converted back and forth
#' X
#' as.data.frame(as.data.list(X))
#'
#' @export
as.data.list.data.frame <- function(object) {
    X <- object
    #TEST
    #grep("\\.[hk][[:digit:]]+$", c("Ta.k1","Ta.k2","I.h1"))
    # Check which columns hold forecasts and must be returned as data.frames in the data.list
    inmsfor <- grep("\\.[hk][[:digit:]]+$", names(X))
    #
    if(length(inmsfor) > 0){
        # Find the names of them
        nmsfor <- unique(unlist(
            getse(strsplit(names(X)[inmsfor], "\\."), 1)
        ))
        # Group all in a list
        # Note that "u.k2y" is matched, but it maybe shouldn't be: grep("\\.k[:digit:]*", c("ij.k1","i","u.k2y"))
        L <- lapply(nmsfor, function(nm){
            return(inmsfor[grep(pst("^",nm), names(X)[inmsfor])])
        })
        names(L) <- nmsfor
        # The vectors (time t, and observations)
        Lobs <- as.list((1:ncol(X))[-inmsfor])
        names(Lobs) <- names(X)[-inmsfor]
    }else{
        # No forecasts found
        L <- list()
        # The vectors (time t, and observations)
        Lobs <- as.list((1:ncol(X)))
        names(Lobs) <- names(X)
    }
    #
    # Combine and sort like the order they came in
    L <- c(L, Lobs)
    L <- L[order(unlist(getse(L, 1)))]
    #
    # Make the data.list
    val <- lapply(L, function(i) {
        tmp <- X[ ,i]
        if(!is.null(dim(tmp))){
            # Its a forecast, hence a data.frame, remove "name." from the column names
            names(tmp) <- getse(strsplit(names(tmp), "\\."), 2)
        }
        return(tmp)
    })
    class(val) <- "data.list"
    return(val)
}
