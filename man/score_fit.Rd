% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_fit.R
\name{score_fit}
\alias{score_fit}
\title{Calculates scores for a forecast model fit.}
\usage{
score_fit(fit, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse)
}
\arguments{
\item{fit}{A model fit}

\item{scoreperiod}{as an index (logical or integer) defining which points to inlude in the score calculation. If NA, the \code{scoreperiod} from the \code{fit$model} object is used.}

\item{usecomplete}{Only use points where}

\item{scorefun}{The score function applied, per default \code{\link{rmse}}.}
}
\value{
A list with:
  - \code{scoreval} is the score value
  - \code{scoreperiod} is the score period used (can be different that the one in arguments \code{fit} or \code{scoreperiod})
  - \code{scorename} is the name of the score function applied
}
\description{
Calculate the score for each horizon for a forecast model fit.
}
\details{
For evaluation of the score on each horizon, as specified in the fit. Use it for a consistent evaluation.
}
\seealso{
\code{\link{rmse}} and \code{\link{score}} which are used in this function.
}
