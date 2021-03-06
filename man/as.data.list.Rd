% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.data.list.R
\name{as.data.list}
\alias{as.data.list}
\title{Convert to data.list class}
\usage{
as.data.list(object)
}
\arguments{
\item{object}{The object to be converted into a data.list}
}
\value{
a value of class data.list
}
\description{
These functions will convert the object into a data.list.
}
\details{
A data.list is simply a list of vectors and data.frames. For the use in the 
onlineforecast package the following format must be kept:

  - t: A vector of time.

  - vectors with same length as t: Holds observations and values synced to time t.

  - data.frames with number of rows as time t: Holds forecasts in each column named by \code{kxx} where \code{xx} is the
                                               horizon, e.g. \code{k0} is synced as observations, and \code{k1} is one-step ahead.
}
\seealso{
\code{For specific detailed info see the children, e.g. \link{as.data.list.data.frame} }

Other as.data.list: 
\code{\link{as.data.list.data.frame}()}
}
\concept{as.data.list}
