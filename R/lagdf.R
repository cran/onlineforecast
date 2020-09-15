## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lagdf

#' Lagging by shifting the values back or fourth always returning a data.frame.
#'
#' This function lags (shifts) the values of the vector. A data.frame is always returned with the columns
#' as the vectors lagged with the values in lagseq. The column names are set to "kxx", where xx are the lag of the column.
#'
#' 
#' @title Lagging which returns a data.frame
#' @param x The vector to be lagged.
#' @param lagseq The integer(s) setting the lag steps.
#' @return A data.frame.
#' @rdname lagdf
#' @seealso \code{\link{lagdf.data.frame}} which is run when \code{x} is a data.frame.
#' @examples
#' # The values are simply shifted
#' # Ahead in time
#' lagdf(1:10, 3)
#' # Back in time
#' lagdf(1:10, -3)
#' # Works but returns a numric
#' lagdf(as.factor(1:10), 3)
#' # Works and returns a character
#' lagdf(as.character(1:10), 3)
#' # Giving several lag values
#' lagdf(1:10, c(1:3))
#' lagdf(1:10, c(5,3,-1))
#'
#' # See also how to lag a forecast data.frame with: ?lagdf.data.frame
#'
#'
#'@export

lagdf <- function(x, lagseq){
    UseMethod("lagdf")
}


#' @export
lagdf.numeric <- function(x, lagseq) {
    ## Return a data.frame
    tmp <- lapply_cbind_df(lagseq, function(lag){
        return(lagvec(x, lag))
    })
    names(tmp) <- pst("k",lagseq)
    return(tmp)
}


#' @export
lagdf.factor <- function(x, lagseq) {
    lagdf.numeric(x, lagseq)
}


#' @export
lagdf.character <- function(x, lagseq) {
    lagdf.numeric(x, lagseq)
}

#' @export
lagdf.logical <- function(x, lagseq) {
    lagdf.numeric(x, lagseq)
}


#' Lagging of a data.frame
#'
#' This function lags the columns with the integer values specified with the argument \code{lagseq}.
#' 
#' @title Lagging of a data.frame
#' @param x The data.frame to have columns lagged
#' @param lagseq The sequence of lags as an integer. Alternatively, as a character "+k", "-k", "+h" or "-h", e.g. "k12" will with "+k" be lagged 12.
#' @return A data.frame with columns that are lagged
#' @rdname lagdf
#' @examples
#' 
#' # dataframe of forecasts
#' X <- data.frame(k1=1:10, k2=1:10, k3=1:10)
#' X
#'
#' # Lag all columns
#' lagdf(X, 1)
#' \dontshow{if(!all(is.na(lagdf(X, 1)[1, ]))){stop("Lag all columns didn't work")}}
#'
#' # Lag each column different steps
#' lagdf(X, 1:3)
#' # Lag each columns with its k value from the column name
#' lagdf(X, "+k")
#' \dontshow{
#'     if(any(lagdf(X, 1:3) != lagdf(X, "+k"),na.rm=TRUE)){stop("Couldn't lag +k")}
#' }
#' # Also works for columns named hxx
#' names(X) <- gsub("k", "h", names(X))
#' lagdf(X, "-h")
#'
#' # If lagseq must have length as columns in X, it doesn't know how to lag and an error is thrown
#' try(lagdf(X, 1:2))
#' 
#' \dontshow{
#' if(!class(lagdf(data.frame(k1=1:10), 2)) == "data.frame"){stop("Trying to lag data.frame with 1 column, but return is not class data.frame")}
#' if(!all(dim(lagdf(data.frame(k1=1:10), "+k")) == c(10,1))){stop("Trying to lag data.frame with 1 column, but return is not class data.frame")}
#' }
#'
#' @export
lagdf.data.frame <- function(x, lagseq) {
    X <- x
    nms <- nams(X)
    if (length(lagseq) == 1) {
        if (lagseq %in% c("+k","+h")) {
            lagseq <- rep(0, length(nms))
            ## lagseq according to the k value of the columnnames
            i <- grep("^[k|h][[:digit:]]+$", nms)
            lagseq[i] <- as.integer(sapply(strsplit(nms[i], "[k|h]"), function(x){ x[length(x)] }))
        } else if (lagseq %in% c("-k","-h")) {
            lagseq <- rep(0, length(nms))
            ## lagseq according to the negative k value of the columnnames
            i <- grep("^[k|h][[:digit:]]+$", nms)
            lagseq[i] <- -as.integer(sapply(strsplit(nms[i], "[k|h]"), function(x){ x[length(x)] }))
        }
    }
    if (length(lagseq) > 1) {
        if(length(lagseq) != ncol(X)){
            stop(pst("Must have same columns as length of lagseq: data.frame has ",ncol(X)," columns and laqseq is of length ",length(lagseq)))
        }else{
            ## lagseq has length equal to the number of columns in X
            X <- as.data.frame(sapply(1:length(lagseq), function(i) {
                lagvec(X[, i], lagseq[i])
            }))
            nams(X) <- nms
         }
    } else {
        ## X is a data.frame, but lag is a factor, so lag all
        lag <- lagseq
        ## If only one row in X given, then X it is a not a data.frame anymore (code above has changed it)
        if(is.vector(X)){
          X <- as.data.frame(lagvec(X, lag))
          nams(X) <- nms
        } else {
            if (lag > 0) {
                X[(lag + 1):nrow(X), ] <- X[1:(nrow(X) - lag), ]
                X[1:lag, ] <- NA
            } else if (lag < 0) {
                lag <- -lag
                X[1:(nrow(X) - lag), ] <- X[(lag + 1):nrow(X), ]
                X[(nrow(X) - lag + 1):nrow(X), ] <- NA
            }
        }
     }
    return(X)
}

#' @export
lagdf.matrix <- function(x, lagseq){
    lagdf.data.frame(x, lagseq)
}

## ## Test
## x <- data.frame(k1=1:5,k2=6:10)
## ##
## lagdf(x, lagseq=1)
## source("nams.R")
## lagdf(as.matrix(x), lagseq=c(1,2))
## ##
## lagdf(x, lagseq="+k")
## lagdf(x, "+k")
## lagdf(x, "-k")

## lagdf.data.table <- function(x, nms, lagseq, per_reference = FALSE) {
##     DT <- x
##     if (!per_reference) {
##         ## Don't do it per reference
##         X <- DT[, ..nms]
##         for (i in 1:length(lagseq)) {
##             if (lagseq[i] > 0) {
##                 X[, `:=`(c(nams(X)[i]), shift(.SD, lagseq[i], NA, "lag")), .SDcols = c(nams(X)[i])]
##             } else if (lagseq[i] < 0) {
##                 X[, `:=`(c(nams(X)[i]), shift(.SD, -lagseq[i], NA, "lead")), .SDcols = c(nams(X)[i])]
##             }
##         }
##         return(X)
##     } else {
##         ## Here also names of the columns to be shifted should be given Do it per
##         ## reference
##         for (i in 1:length(lagseq)) {
##             if (lagseq[i] > 0) {
##                 DT[, `:=`(c(nms[i]), shift(.SD, lagseq[i], NA, "lag")), .SDcols = c(nms[i])]
##             } else if (lagseq[i] < 0) {
##                 DT[, `:=`(c(nms[i]), shift(.SD, -lagseq[i], NA, "lead")), .SDcols = c(nms[i])]
##             }
##         }
##         invisible(NULL)
##     }
## }

