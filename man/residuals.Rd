% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuals.R
\name{residuals.data.frame}
\alias{residuals.data.frame}
\alias{residuals.matrix}
\alias{residuals.forecastmodel_fit}
\title{Calculate the residuals given a forecast matrix and the observations.}
\usage{
\method{residuals}{data.frame}(object, y, ...)

\method{residuals}{matrix}(object, y, ...)

\method{residuals}{forecastmodel_fit}(object, ...)
}
\arguments{
\item{object}{The value from a fit a forecastmodel (currently \code{\link{lm_fit}} or \code{\link{rls_fit}}.}

\item{y}{The observations vector.}

\item{...}{Not used.}
}
\value{
A data.frame with the residuals for each horizon.
}
\description{
Calculate the residuals given a forecast matrix and the observations.
}
\details{
Simply give the forecast matrix and the observations to get the residuals for each horizon in the forecast matrix.

The residuals returned are synced with the observations (i.e. k0) and the columns are names "hxx" (not kxx) to indicate this and will not be lagged in \code{\link{plot_ts}()}.
}
\examples{
# Just a vector to be forecasted
n <- 100
D <- data.list()
D$t <- 1:n
D$y <- c(filter(rnorm(n), 0.95, "recursive"))
plot(D$y, type="l")

# Generate a forecast matrix with a simple persistence model
D$Yhat <- persistence(D$y, kseq=1:4)

# The residuals for each horizon
D$Resid <- residuals(D$Yhat, D$y)
D$Resid
# Note the names of the columns
names(D$Resid)
# which means that they are aligned with the observations and will not be lagged in the plot
plot_ts(D, c("y|Yhat","Resid"))

# Check that it matches (the forecasts is lagged in the plot_ts
# such that the forecast for t+k is at t+k (and not t))
plot_ts(D, c("y|Yhat","Resid"), xlim=c(1,10), kseq=1,
        plotfun=function(x,...){lines(x,...,type="b")})

# Just for fun, see the auto-correlation function of the persistence 
acf(D$Resid$h1, na.action=na.pass)
acf(D$Resid$h4, na.action=na.pass)

}
