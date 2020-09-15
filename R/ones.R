# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?one

#' Returns a data.frame of ones which can be used in forecast model inputs
#'
#' The function returns ones which can be used to generate ones, e.g. to be used as a intercept for a model.
#'
#' See vignettes 'setup-data' and 'setup-and-use-model'.
#'
#' @title Create ones for model input intercept
#' @return A data.frame of ones
#' @name one
#' @examples
#'
#' # A model
#' model <- forecastmodel$new()
#' # Use the function in the input definition
#' model$add_inputs(mu = "one()")
#' # Set the forecast horizons
#' model$kseq <- 1:4
#' # During the transformation stage the one will be generated for the horizons
#' model$transform_data(subset(Dbuilding, 1:7))
#'
#' @export

one <- function(){
    # To find kseq, get the model (remember it is call per reference, so don't change it without cloning)
    model <- get("self", parent.env(parent.frame(4)))
    # Get the data to find the all the names with k in data
    data <- get("data", parent.env(parent.frame()))
    n <- length(data$t)
    # Generate the matrix of one and return it as a data.frame
    as.data.frame(matrix(1, nrow=n, ncol=length(model$kseq), dimnames=list(NULL, pst("k",model$kseq))))
}
