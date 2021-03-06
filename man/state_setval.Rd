% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state_setval.R
\name{state_setval}
\alias{state_setval}
\title{Set a state value to be kept for next the transformation function is called.}
\usage{
state_setval(val)
}
\arguments{
\item{val}{The value to set and kept for next call.}
}
\description{
Set a state value to be kept for next the transformation function is called.
}
\details{
Transformation functions (e.g. \code{\link{lp}}, \code{\link{fs}}, \code{\link{bspline}}) can need to keep a state value between calls, e.g. when new data arrives and must be transformed. This function is used to setting the state values set in last call to the function.

Uses the \code{input_class$state_getval()}.
}
\examples{

# See how it can be used in lp, which needs to save the state of the filter
# Note how it is not needed to do anything else than getting and setting the state
# in transformations (model$transform_data()), then multiple transformation functions can be called,
# but they are always in the same order, so the state (set,get) functions keep a counter internally
# to make sure that the correct values are set and returned when called again.
lp


}
\seealso{
\code{\link{state_setval}()} for setting the state value and \code{\link{input_class}}.
}
