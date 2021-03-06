% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rls_prm.R
\name{rls_prm}
\alias{rls_prm}
\title{Function for generating the parameters for RLS regression}
\usage{
rls_prm(lambda)
}
\arguments{
\item{lambda}{The forgetting factor}
}
\value{
A list of the parameters
}
\description{
Function for generating the parameters for RLS regression
}
\details{
The RLS needs only a forgetting factor parameter.
}
\examples{

# Take data
D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
D$y <- D$heatload
D$scoreperiod <- in_range("2010-12-20", D$t)
# Define a simple model 
model <- forecastmodel$new()
model$add_inputs(Ta = "Ta", mu = "one()")
model$kseq <- 1:6

# Here the expression which sets the parameters is defined
model$add_regprm("rls_prm(lambda=0.99)")
model$regprmexpr

# These will fit with lambda=0.99
rls_fit(prm=NA, model, D)
rls_fit(prm=c(lambda=0.99), model, D)

# The expression is evaluated when the model is fitted
rls_fit(prm=c(lambda=0.85), model, D)

# What happens is simply that the expression was manipulated
model$regprmexpr
model$regprm

# Same change could be done by
model$regprm <- list(lambda=0.3)
model$regprm
val <- rls_fit(prm=NA, model, D)

}
