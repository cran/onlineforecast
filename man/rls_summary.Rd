% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rls_summary.R
\name{rls_summary}
\alias{rls_summary}
\title{Print summary of an onlineforecast model fitted with RLS}
\usage{
rls_summary(
  object,
  scoreperiod = NA,
  scorefun = rmse,
  usecomplete = TRUE,
  printit = TRUE,
  ...
)
}
\arguments{
\item{object}{of class \code{rls_fit}, so a fit calculated by \code{\link{rls_fit}}.}

\item{scoreperiod}{logical (or index). If this scoreperiod is given, then it will be used over the one in the fit.}

\item{scorefun}{The score function to be applied on each horizon.}

\item{usecomplete}{Use on the set of observations which is complete on all horizons.}

\item{printit}{Print the result.}

\item{...}{Not used.}
}
\value{
A list of:

    - scorefun.

    - scoreval (value of the scorefun for each horizon).

    - scoreperiod is the scoreperiod used.
}
\description{
The summary of an onlineforecast model fitted with RLS with simple stats providing a simple overview.
}
\details{
The following is printed:

* The model.

* Number of observations included in the scoreperiod.

* RLS coefficients summary statistics for the estimated coefficient time series (since observations are correlated, then usual statistics cannot be applied directly):

    - mean: the sample mean of the series.

    - sd: sample standard deviation of the series.

    - min: minimum of the series.

    - max: maximum of the series.

* Scorefunction applied for each horizon, per default the RMSE.
}
\examples{

# Take data
D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
D$y <- D$heatload
D$scoreperiod <- in_range("2010-12-20", D$t)
# Define a model
model <- forecastmodel$new()
model$add_inputs(Ta = "Ta",
                 mu = "one()")
model$add_regprm("rls_prm(lambda=0.99)")
model$kseq <- 1:6
# Fit it
fit <- rls_fit(prm=c(lambda=0.99), model, D)

# Print the summary
summary(fit)
# We see:
#   - The model (output, inputs, lambda)
#   - The Ta coefficient is around -0.12 in average (for all horizons) with a standard dev. of 0.03,
#     so not varying extremely (between -0.18 and -0.027).
#   - The intercept mu is around 5.5 and varying very little.
#   - The RMSE is around 0.9 for all horizons.

# The residuals and coefficient series can be seen by
plot_ts(fit)

}
