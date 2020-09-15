## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lp

#' First-order low-pass filtering of a time series vector.
#'
#' This function applies a first order unity gain low-pass filter to the columns of \code{X}.
#' The low-pass filter is applied to each column seperately. The stationary gain of the filter i one.
#'
#' If a list of dataframes (or matrices) is given, then the low-pass filtering is recursively applied on each.
#' 
#' @title First-order low-pass filtering
#' @param X Dataframe or matrix (or list of them) of forecasts in columns to be low-pass filtered.
#' @param a1 The low-pass filter coefficient.
#' @param usestate logical: Use the state kept in the model$input? if \code{lp()} is used outside model$transform_data(), then it must be set to FALSE, otherwise the input$state (which is not there) will be read leading to an error.
#' @return The low-pass filtered dataframe (as a matrix)
#' @examples
#' # Make a dataframe for the examples
#' X <- data.frame(k1=rep(c(0,1),each=5))
#' X$k2 <- X$k1
#' Xf <- lp(X, 0.5, usestate=FALSE)
#' Xf
#'
#' # See the input and the low-pass filtered result
#' plot(X$k1)
#' lines(Xf[ ,"k1"])
#' # Slower response with higher a1 value
#' lines(lp(X, 0.8, usestate=FALSE)[ ,"k1"])
#'
#' # If a list of dataframes is given, then lp() is recursively applied on each
#' lp(list(X,X), 0.5, usestate=FALSE)
#'
#' 
#' @export

lp <- function(X, a1, usestate = TRUE) {
    ## 
    if (class(X) == "list") {
        ## If only one coefficient, then repeat it
        if (length(a1) == 1) {
            a1 <- rep(a1, length(X))
        }
        ## Call again for each element
        val <- lapply(1:length(X), function(i) {
            lp(X[[i]], a1[i], usestate)
        })
        nams(val) <- nams(X)
        return(val)
    }
    ## Get the state value saved last time Get the value if it is not the first time,
    ## it can be a variable of any class
    yInit <- rep(NA,ncol(X))
    if(usestate){
        yInit <- state_getval(initval = yInit)
    }
    ## Carry out the function, insert the init value and remove afterwards
    val <- apply(rbind(yInit, X), 2, lp_vector_cpp, a1 = a1)[-1, , drop = FALSE]
    ## Keep the state value
    if(usestate){
        state_setval(val[nrow(X), ])
    }
    ## Return the value
    return(val)
}


lp_vector <- function(x, a1) {
    ## Make a 1'st order low pass filter as (5.3) p.46 in the HAN report.
    y <- numeric(length(x))
    oma1 <- 1 - a1
    ## First value in x is the init value
    y[1] <- x[1]
    ## 
    for (i in 2:length(x)) {
        if (is.na(y[i - 1])) {
            y[i] <- x[i]
        } else {
            y[i] <- a1 * y[i - 1] + (1 - a1) * x[i]
        }
    }
    ## Return (afterwards the init value y[1], must be handled)
    return(y)
}


## ## Test ##x <- c(rep(0,10),rep(1,10)) x <- rnorm(200) x[5] <- NA lp_vector(x, 0.8)
## lp_vector_cpp(x, 0.8)

## plot(x) lines(lp_vector_cpp(x, 0.8))

## require(microbenchmark) microbenchmark( lp_vector(x, 0.8), lp_vector_cpp(x, 0.8) )

