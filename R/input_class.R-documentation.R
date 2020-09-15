# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?input_class

#' R6 class for for forecastmodel inputs
#' 
#' Holds variables and functions needed for an input, as added by \code{\link{forecastmodel}$add_inputs()}.
#'
#' @title Class for forecastmodel inputs
#' @name input_class
#' @details
#' Details of the class.
#'
#'
#' 
#' @section Public fields:
#'
#'      - expr = NA: The expression as string for transforming the input.
#' 
#'      - state_L = list(): The list holding potential state values kept by the function evaluated in the expression.
#'
#'      - state_i = integer(1):  index counter for the state list.
#'
#'
#----------------------------------------------------------------
#' @section Public methods:
#' All public functions are described below and in examples a section for each is included:
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$new(expr)}:
#' Create a new input with the expression \code{expr}.
#' 
#' @examples
#' # new:
#'
#' # An input is created in a forecastmodel
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "lp(Ta, a1=0.9)")
#' # The 'inputs' is now a list 
#' model$inputs
#' # With the input object
#' class(model$inputs[[1]])
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$evaluate(data}:
#' Generate (transform) the input by evaluating the expr with the \code{data} (data.list) attached.
#'
#' @examples
#'
#' # Now the transformation stage can be carried out to create the regression stage data
#' # Take a data.list subset for the example
#' D <- subset(Dbuilding, 1:10, kseq=1:4)
#' # Transform the data
#' model$inputs[[1]]$evaluate(D)
#' # What happens is simply that the expression is evaluated with the data
#' # (Note that since not done in the model some functions are missing)
#' eval(parse(text=model$inputs[[1]]$expr), D)
#'
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$state_reset()}:
#' Each function in the expressions (lp, fs, etc.) have the possibility to save a state, which can be read next time the are called.
#' 
#' Reset the state by deleting \code{state_L} and setting \code{state_i} to 0.
#'
#'
#' # After running
#' model$inputs[[1]]$evaluate(D)
#' # the lp() has saved it's state for next time
#' model$inputs[[1]]$state_L
#' # New data arrives
#' Dnew <- subset(Dbuilding, 11, kseq=1:4)
#' # So in lp() the state is read and it continues
#' model$inputs[[1]]$evaluate(Dnew)
#'
#' # If we want to reset the state, which is done in all _fit() functions (e.g. rls_fit), such that all transformations starts from scratch
#' # Reset the state
#' model$inputs[[1]]$evaluate(D)
#' # Test resetting
#' model$inputs[[1]]$state_reset()
#' # Now there is no state
#' model$inputs[[1]]$evaluate(Dnew)
#' # So lp() starts by taking the first data point
#' Dnew$Ta
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$state_getval(initval)}:
#' Get the saved value in state. This function can be used in the beginning of transformation functions to get the current state value.
#' First time called return the \code{initval}.
#'
#' Note that since all transformation functions are called in the same order,
#' then the state can be read and saved by keeping a counter internally, the value is saved in the field $state_i.
#'
#' See example of use in \code{\link{lp}()}.
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$state_setval(val)}:
#' Set the state value, done in the end of a transformation function, see above.
#'
#' See example of use in \code{\link{lp}()}.
#' 
#----------------------------------------------------------------
NULL
# Don't delete the NULL above
