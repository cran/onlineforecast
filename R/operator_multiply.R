## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?"%**%"

#' Multiplication of each element in a list (x) with y
#'
#' Each element of x is multiplied with y using the usual elementwise '*' operator.
#'
#' Typical use is when a function, e.g. \code{\link{bspline}()}, returns a list of matrices (e.g. one for each base spline) and they should individually be multiplied with y (a vector, matrix, etc.).
#' 
#' Since this is intended to be used for forecast models in the transformation stage 
#' then there are some percularities:
#'
#' If the number of columns or the names of the columns are not equal for one element in x
#' and y, then only the columns with same names are used, hence the resulting matrices can be
#' of lower dimensions.
#'
#' See the example \url{https://onlineforecasting.org/examples/solar-power-forecasting.html} where the operator is used.
#' 
#' @title Multiplication of list with y, elementwise
#' @param x a list of matrices, data.frames, etc.
#' @param y a vector, data.frame or matrix
#' @return A list of same length of x
#' @examples
#'
#' x <- list(matrix(1:9,3), matrix(9:1,3))
#' x
#' 
#' y <- matrix(2,3,3)
#' y
#'
#' x %**% y
#'
#' y <- 1:3
#'
#' x %**% y
#'
#' # Naming percularity
#' nams(x[[1]]) <- c("k1","k2","k3")
#' nams(x[[2]]) <- c("k2","k3","k4")
#' y <- matrix(2,3,3)
#' nams(y) <- c("k1","k3","k7")
#'
#' # Now the only the horizons matching will be used
#' x %**% y
#' 
#' @export

"%**%" <- function(x, y) {
    if( is.null(dim(y)) ){
        ## y is not matrix like
        lapply(x, function(xx) {
            xx * y
        })
    }else{
        ## y is matrix like
        lapply(x, function(xx) {
            ## Check if different horizon k columns
            colmatch <- TRUE
            if (ncol(xx) != ncol(y)) {
                colmatch <- FALSE
            }else if(any(nams(xx) != nams(y))){
                colmatch <- FALSE
            }
            if(!colmatch){
                ## Not same columns, take only the k in both
                nms <- nams(xx)[nams(xx) %in% nams(y)]
                xx <- xx[, nms]
                y <- y[, nms]
            }
            ## Now multiply
            val <- xx * y
            ## Must be data.frame
            if( is.null(dim(val)) ){
                val <- data.frame(val)
                nams(val) <- nms
            }
            return(val)
        })
    }
}
