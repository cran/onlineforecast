% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ts.R, R/plotly_ts.R
\name{plot_ts}
\alias{plot_ts}
\alias{plot_ts.data.list}
\alias{plot_ts.data.frame}
\alias{plot_ts.matrix}
\alias{plot_ts.rls_fit}
\alias{plotly_ts}
\title{Time series plotting}
\usage{
plot_ts(
  object,
  patterns = ".*",
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = "",
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  plotit = TRUE,
  p = NA,
  ...
)

\method{plot_ts}{data.list}(
  object,
  patterns = ".*",
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = "",
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  plotit = TRUE,
  p = NA,
  kseq = NA,
  ...
)

\method{plot_ts}{data.frame}(
  object,
  patterns = ".*",
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = NA,
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  plotit = TRUE,
  p = NA,
  namesdata = NA,
  ...
)

\method{plot_ts}{matrix}(
  object,
  patterns = ".*",
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = NA,
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  plotit = TRUE,
  p = NA,
  namesdata = NA,
  ...
)

\method{plot_ts}{rls_fit}(
  object,
  patterns = c("^y$|^Yhat$", "^Residuals$", "CumAbsResiduals$", pst("^",
    names(fit$Lfitval[[1]]), "$")),
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = "",
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  plotit = TRUE,
  p = NA,
  kseq = NA,
  ...
)

plotly_ts(
  object,
  patterns = ".*",
  xlim = NA,
  ylims = NA,
  xlab = "",
  ylabs = NA,
  mains = "",
  mainouter = "",
  legendtexts = NA,
  xat = NA,
  usely = FALSE,
  p = NA,
  ...
)
}
\arguments{
\item{object}{A \code{data.list} or \code{data.frame} with observations and forecasts, note diffe}

\item{patterns}{See \code{\link{plot_ts}}. The default pattern finds the generated series in the function, '!!RLSinputs!!' will be replaced with the names of the RLS inputs (regression stage inputs).}

\item{xlim}{The time range as a character of length 2 and form "YYYY-MM-DD" or POSIX. Date to start and end the plot.}

\item{ylims}{The \code{ylim} for each plot given in a list.}

\item{xlab}{A character with the label for the x-axis.}

\item{ylabs}{A character vector with labels for the y-axes.}

\item{mains}{A character vector with the main for each plot.}

\item{mainouter}{A character with the main at the top of the plot (can also be added afterwards with \code{title(main, outer=TRUE)}).}

\item{legendtexts}{A list with the legend texts for each plot (replaces the names of the variables).}

\item{xat}{POSIXt specifying where the ticks on x-axis should be put.}

\item{usely}{If TRUE then plotly will be used.}

\item{plotit}{If FALSE then the plot will not be generated, only data returned.}

\item{p}{The plot_ts parameters in a list, as generated with the function \code{\link{par_ts}()}.}

\item{...}{Parameters passed to \code{\link{par_ts}}, see the list of parameters in \code{?\link{par_ts}}.}

\item{kseq}{For \code{class(object)=="data.list"} an integer vector, default = NA. Control which forecast horizons to include in the plots. If NA all the horizons will be included.}

\item{namesdata}{For \code{class(object)=="data.frame"} a character vector. Names of columns in object to be searched in, instead of \code{names(object)}.}

\item{fit}{An \code{rls_fit}.}
}
\value{
A list with a data.frame with the data for each plot, if usely=TRUE, then a list of the figures (drawn with print(subplot(L, shareX=TRUE, nrows=length(L), titleY = TRUE))).

The plotted data in a \code{data.list}.
}
\description{
Plot time series of observations and forecasts (lagged to be aligned in time).

Plot forecasts, residuals, cumulated residuals and RLS coefficients

Simply the same as \code{\link{plot_ts}()} with \code{usely=TRUE}, such that plotly is used.
}
\details{
Generates time series plots depending on the variables matched by each regular expression given in the \code{patterns} argument.

The forecasts matrices in the \code{data.list} given in \code{object} will be lagged to be aligned in time (i.e. k-step forecasts will be lagged by k).

Use the plotly package if argument \code{usely} is TRUE, see \code{\link{plotly_ts}()}.

A useful plot for residual analysis and model validation of an RLS fitted forecast model.

All parameters, except those described below, are simply passed to \code{\link{plot_ts}()}.

The \code{plotly} package must be installed and loaded.

Note that the plot parameters set with \code{\link{par_ts}()} have no effect on the \code{plotly} plots.

See \url{https://onlineforecasting.org/vignettes/nice-tricks.html}.
}
\examples{

# Time series plots for \code{data.list}, same as for \code{data.frame} except use of \code{kseq}
D <- Dbuilding
plot_ts(D, c("heatload","Ta"), kseq=c(1,24))
# Make two plots (and set the space for the legend)
plot_ts(D, c("heatload","Ta"), kseq=c(1,24), legendspace=11)
# Only the Ta observations 
plot_ts(D, c("heatload","Taobs$"), kseq=c(1,24), legendspace=11)

# Give labels
plot_ts(D, c("heatload","Ta"), kseq=c(1,24), xlab="Time", ylabs=c("Heat (kW)","Temperature (C)"))
# Mains (see mainsline in par_ts())
plot_ts(D, c("heatload","Ta"), kseq=c(1,24), mains=c("Heatload","Temperature"), mainsline=c(-1,-2))

# Format of the xaxis (see par_ts())
plot_ts(D, c("heatload","Ta"), kseq=c(1,24), xaxisformat="\%Y-\%m-\%d \%H:\%m")

# Return the data, for other plots etc.
L <- plot_ts(D, c("heatload","Ta"), kseq=c(1,24))
names(L[[1]])
names(L[[2]])



# Fit a model (see vignette 'setup-and-use-model'
D <- Dbuilding
D$scoreperiod <- in_range("2010-12-22", D$t)
model <- forecastmodel$new()
model$output = "heatload"
model$add_inputs(Ta = "Ta",
                 mu = "one()")
model$add_regprm("rls_prm(lambda=0.9)")
model$kseq <- c(3,18)
fit1 <- rls_fit(NA, model, D, returnanalysis = TRUE)

# Plot it
plot_ts(fit1)

# Return the data
Dplot <- plot_ts(fit1)

# The RLS coefficients are now in a nice format
head(Dplot$mu)


# See the website link above

}
\seealso{
\code{\link{par_ts}} for setting plot control parameters.

\code{\link{regex}} for regular expressions to select which variables to plot.

\code{\link{plot_ts}}.
}
