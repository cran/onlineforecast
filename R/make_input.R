# Do this in a separate tmp.R file to check the documentation
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?make_input

#' Make a forecast matrix (as data.frame) from observations.
#'
#' This function creates a data.frame with columns for each horizon such that it can be 
#' added to a data.list and used in a forecast model.
#' 
#' @param observations vector of observations.
#' @param kseq vector of integers, respresenting the desired "k-steps ahead".
#' @return Returns a forecast matrix (as a data.frame) with simply the observation vector copied to each column.
#' @examples
#'
#' # Data for example
#' D <- subset(Dbuilding, c("2010-12-15","2010-12-20"))
#' 
#' # Generate the input
#' make_input(D$heatload, 1:4)
#'
#' # Set is in D, such that it can be used in input expressions (e.g. by model$add_inputs(AR = "Ar0")
#' D$AR0 <- make_input(D$heatload, 1:36)
#' 
#' @export
make_input <- function(observations, kseq){
    val <- sapply(kseq, function(k){
        observations
    })
    ## set row and column names
    nams(val) <- paste0('k', kseq)
    return( as.data.frame(val) )
}
