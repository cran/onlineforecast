# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?state_getval

#' Get the state value kept in last call to the transformation function.
#'
#' Transformation functions (e.g. \code{\link{lp}}, \code{\link{fs}}, \code{\link{bspline}}) can need to keep a state value between calls, e.g. when new data arrives and must be transformed. This function is used to getting the state values set in last call to the function.
#'
#' Uses the \code{input_class$state_getval()}.
#' 
#' @title Get the state value kept in last call.
#' @param initval If no state was kept, then this init value is returned.
#' @return The state value, but if not found, then the initval.
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
state_getval <- function(initval) {
    # Find the environment (frame) with the model
    # Must be done with for loop, if done with lapply the order of parents change
    # When this function is called inside a transformation function, then 
    
    # browser()
    # # Debugging to find the content of the environments
    # for(i in sys.parents()){
    #     nms <- ls(parent.frame(i+1))
    #     print(nms)
    # }
    #

    # This seems to work all time
    # When "render("building-heat-load-forecasting.Rmd")" the "input" is not there
    # Running tests, then the c("data","self") is not there
    # Sometimes the input object is the only available
    for(i in sys.parents()){
        nms <- ls(parent.frame(i+1))
        if(length(nms) == 1){
            if(nms == "input"){
                return(parent.frame(i+1)$input$state_getval(initval))
            }
        }else if(length(nms) == 2){
            if(all(nms[1:2] == c("data","self"))){
                return(parent.frame(i+1)$self$state_getval(initval))
            }
        }
    }
    # If made it to here, the input was not found, so throw a meaningful error
    warning("In state_getval() the object of class input was not found in the parent environments. The initval was returned.")
    return(initval)
}
