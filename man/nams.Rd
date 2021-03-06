% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nams.R
\name{nams}
\alias{nams}
\alias{nams<-}
\title{Return the column names}
\usage{
nams(x)

nams(x) <- value
}
\arguments{
\item{x}{The matrix or data.frame to set the column names for.}

\item{value}{The names to be given.}
}
\description{
Return the column names of a dataframe or a matrix.
}
\details{
Simply to have a single function for returning the column names, instead of
\code{colnames()} for a \code{matrix} and \code{names()} for a \code{data.frame}).
}
\examples{

# Generate a matrix
X <- matrix(1, nrow=2, ncol=3)
colnames(X) <- c("c1","c2","c3")
D <- as.data.frame(X)

# Annoyingly this fails (for a matrix)
\dontrun{names(X)}
# Could use this everywhere
colnames(D)
# but this is shorter
nams(X)
nams(D)

# Also for assignment
nams(D) <- c("x1","x2","x3")
nams(D)

}
