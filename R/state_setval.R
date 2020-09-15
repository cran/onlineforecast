#' Set a state value to be kept for next the transformation function is called.
#'
#' Transformation functions (e.g. \code{\link{lp}}, \code{\link{fs}}, \code{\link{bspline}}) can need to keep a state value between calls, e.g. when new data arrives and must be transformed. This function is used to setting the state values set in last call to the function.
#'
#' Uses the \code{input_class$state_getval()}.
#' 
#' @title Set a state value to be kept for next the transformation function is called.
#' @param val The value to set and kept for next call.
#' @seealso \code{\link{state_setval}()} for setting the state value and \code{\link{input_class}}.
#' @examples
#'
#' # See how it can be used in lp, which needs to save the state of the filter
#' # Note how it is not needed to do anything else than getting and setting the state
#' # in transformations (model$transform_data()), then multiple transformation functions can be called,
#' # but they are always in the same order, so the state (set,get) functions keep a counter internally
#' # to make sure that the correct values are set and returned when called again.
#' lp
#' 
#' 
state_setval <- function(val) {
    ## Find the environment (frame) with the model
    ## Must be done with for loop, if done with lapply the order of parents change

    ## Old way...stopped working in tests, don't know why
    ## browser()
    ## for(i in sys.parents()){
    ##     nms <- ls(parent.frame(i+1))
    ##     print(nms)
    ##     ## if(length(nms) == 2){
    ##     ##     if(all(nms[1:2] == c("data","self"))){
    ##     ##         break
    ##     ##     }
    ##     ## }
    ## }
    ## ## Set the values and return
    ## parent.frame(i+1)$self$state_setval(val)

        ## This seems to work all time
    ## When "render("building-heat-load-forecasting.Rmd")" the "input" is not there
    ## Running tests, then the c("data","self") is not there
    ## Sometimes the input object is the only available
    for(i in sys.parents()){
        nms <- ls(parent.frame(i+1))
        if(length(nms) == 1){
            if(nms == "input"){
                return(parent.frame(i+1)$input$state_setval(val))
            }
        }else if(length(nms) == 2){
            if(all(nms[1:2] == c("data","self"))){
                return(parent.frame(i+1)$self$state_setval(val))
            }
        }
    }
    ## If made it to here, the input was not found, so throw a meaningful error
    warning("In state_setval() the object of class input was not found in the parent environments, so the state value could not be updated.")
}
