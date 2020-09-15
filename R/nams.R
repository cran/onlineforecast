# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?nams

#' Return the column names of a dataframe or a matrix.
#'
#' Simply to have a single function for returning the column names, instead of
#' \code{colnames()} for a \code{matrix} and \code{names()} for a \code{data.frame}).
#' 
#' @title Return the column names
#' @param x The matrix or data.frame to get the column names from.
#' @examples
#'
#' # Generate a matrix
#' X <- matrix(1, nrow=2, ncol=3)
#' colnames(X) <- c("c1","c2","c3")
#' D <- as.data.frame(X)
#' 
#' # Annoyingly this fails (for a matrix)
#' \dontrun{names(X)}
#' # Could use this everywhere
#' colnames(D)
#' # but this is shorter
#' nams(X)
#' nams(D)
#'
#' # Also for assignment
#' nams(D) <- c("x1","x2","x3")
#' nams(D)
#'
#' @export
nams <- function(x) {
    if(is.matrix(x)){
        colnames(x)
    } else {
        names(x)
    }
}


#' @param x The matrix or data.frame to set the column names for.
#' @param value The names to be given.
#' @rdname nams
#' @export
`nams<-` <- function(x, value) {
    if(is.matrix(x)){
        colnames(x) <- value
    } else {
        names(x) <- value
    }
  x
}
