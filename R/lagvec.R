#' Lag by shifting the vecter
#'
#' A positive value of \code{lag} shifts the values to the right in the vector.
#' 
#' @title Lag by shifting
#' @param x The vector to lag
#' @param lag (integer) The steps to lag.
#' @return The shifted vector
#'
#' @examples
#'
#' # The values are simply shifted
#' # Ahead in time
#' lagvec(1:10, 3)
#' # Back in time
#' lagvec(1:10, -3)
#' # Works but returns a numric
#' lagvec(as.factor(1:10), 3)
#' # Works and returns a character
#' lagvec(as.character(1:10), 3)
#' 
#' @export

lagvec <- function(x, lag){
    if (lag > 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(rep(NA, lag), x[1:(length(x) - lag)]))
    }else if(lag < 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(x[(abs(lag) + 1):length(x)], rep(NA, abs(lag))))
    }else{
        ## lag = 0, return x
        return(x)
    }
}
