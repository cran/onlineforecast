# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?forecastmodel

#' R6 class for a forecastmodel
#' 
#' This class holds the variables and functions needed for defining and setting up a forecast model - independent of the fitting scheme.
#' See the vignettes on how to setup and use a model and the website \url{https://onlineforecasting.org} for more info.
#'
#' @title Class for forecastmodels
#' @name forecastmodel
#' @details
#' 
#' Holds all the information needed independently of the fitting scheme (e.g. lm_fit or rls_fit), see the fields and functions below.
#'
#' The fields are separated into:
#'   - Fields for setting up the model
#'   - Fields used when fitting (e.g. which horizons to fit for is set in \code{kseq}
#'
#' See the fields description below.
#' 
#' Note, it's an R6 class, hence an object variable is a pointer (reference), which means two important points:
#'  - In order to make a copy, the function clone_deep() must be used (usually \code{clone(deep=TRUE)}, but that will end in an infinite loop).
#'  - It can be manimulated directly in functions (without return). The code is written such that no external functions manipulate the model object, except for online updating.
#'
#' For online updating (i.e. receiving new data and updating the fit), then the model definition and the data becomes entangled, since transformation functions like low-pass filtering with \code{\link{lp}()} requires past values.
#' See the vignette ??(ref to online vignette, not yet available) and note that \code{\link{rls_fit}()} resets the state, which is also done in all \code{xxx_fit} functions (e.g. \code{\link{rls_fit}}.
#'
#' 
#' @section Public fields used for setting up the model:
#'
#'     - output = NA, character: Name of the output.
#'
#'     - inputs = list(), add them with add_inputs(): List of inputs (which are R6 objects) (note the "cloning of list of reference objects" issue below in deep_clone function)
#'
#'     - regprmexpr = NA: The expression (as character) used for generating the regprm, e.g. "\code{\link{rls_prm}()}" for RLS.
#'
#'     - regprm = list(): Regression parameters calculated by evaluating the \code{regprmexpr}.
#'
#'     - prmbounds = as.matrix(data.frame(lower=NA, init=NA, upper=NA)): The bounds for optimization of the parameters, e.g. with \code{\link{rls_optim}()}.
#'
#'     - outputrange = NA, numeric vector of length 2: Limits of the predictions cropped in the range, e.g. outputrange = c(0,Inf) removes all negative output predictions.
#'
#'
#' @section Public fields used when the model is fitted:
#'
#'     - kseq = NA: The horizons to fit for.
#'
#'     - p = NA: The (transformation stage) parameters used for the fit.
#'
#'     - Lfits = list(): The regression fits, one for each k in kseq (simply a list with the latest fit).
#'
#'     - datatr = NA: Transformed input data (data.list with all inputs for regression)
#'
#'
#----------------------------------------------------------------
#' @section Public methods:
#' All public functions are described below and in examples a section for each is included:
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$new()}:
#' Create a new `forecastmodel` object.
#' 
#' Returns a forecastmodel object.
#' @examples
#' # New object
#' model <- forecastmodel$new()
#'
#' # Print it
#' model
#'
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$add_inputs(...)}:
#'         Add inputs to the model.
#'
#'         - \code{...}: The inputs are given as arguments, see examples.
#'
#' @examples
#'
#' # Add model inputs
#' model$add_inputs(Ta = "lp(Ta)")
#' # See it
#' model$inputs
#' # Update to use no low-pass filter
#' model$add_inputs(Ta = "Ta")
#' model$inputs
#' # Add another
#' model$add_inputs(I = "lp(I)")
#' model$inputs
#'
#' # Simply a list, so manipulate directly
#' class(model$inputs$Ta)
#' model$inputs$Ta$expr <- "lp(Ta, a1=0.9)"
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$add_regprm(regprm_expr)}:
#' Add expression (as character) which generates regression parameters.
#'
#' @examples
#'
#' # Add the parameters for the regression stage
#' model$add_regprm("rls_prm(lambda=0.99)")
#' # The evaluation is a list, which is set in
#' model$regprm
#' 
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$add_prmbounds(...)}:
#' Add the transformation parameters and bounds for optimization.
#'
#' @examples
#'
#' # Set the lambda to be optimized between 0.9 and 0.999, starting at 0.99
#' model$add_prmbounds(lambda = c(0.9, 0.99, 0.999))
#' # Note the "__" syntax to set parameters for inputs: "input__prm"
#' model$add_prmbounds(Ta__a1 = c(0.8, 0.95, 0.99))
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$get_prmbounds(...)}:
#' Get the transformation parameter bounds, used by optimization functions e.g. \code{\link{rls_optim}()}.
#'
#' @examples
#' 
#' # Get the lower bounds
#' model$get_prmbounds("lower")
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$insert_prm(prm)}:
#' Insert the transformation parameters prm in the input expressions and regression expressions, and keep them (simply string manipulation).
#'
#' @examples
#' 
#' # Insert the init parameters
#' prm <- model$get_prmbounds("init")
#' prm
#' # Before
#' model$inputs$Ta$expr
#' # After
#' model$insert_prm(prm)
#' model$inputs$Ta$expr
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$transform_data(data)}:
#' Function for transforming the input data to the regression stage input data (see \code{vignette("setup-data", package = "onlineforecast")}).
#'
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$reset_state()}:
#' Resets the input states and stored data for iterative fitting (datatr rows and yAR) (see ??(ref to online updating vignette, not yet available).
#'
#----------------------------------------------------------------

#----------------------------------------------------------------
#' @section \code{$check(data = NA)}:
#' Check if the model is setup correctly.
#'
#' @examples
#' 
#' # Check if the model is setup and can be used with a given data.list
#' # An error is thrown
#' try(model$check(Dbuilding))
#' # Add the model output
#' model$output <- "heatload"
#' # Still not error free
#' try(model$check(Dbuilding))
#' # Add the horizons to fit for
#' model$kseq <- 1:4
#' # Finally, no errors :)
#' model$check(Dbuilding)
NULL
# Don't delete the NULL above
