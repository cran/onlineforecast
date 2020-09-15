## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?plotly_ts
#?plotly_ts.data.frame

#' Plot time series of observations and predictions, with correct lagging for onlineforecast type of data structures.
#'
#' Simply the same as \code{\link{plot_ts}()} with \code{usely=TRUE}, such that plotly is used.
#'
#' The \code{plotly} package must be installed and loaded.
#'
#' Note that the plot parameters set with \code{\link{par_ts}()} have no effect on the \code{plotly} plots.
#'
#' See \url{https://onlineforecasting.org/vignettes/nice-tricks.html}.
#' 
#' @rdname plot_ts
#' @examples
#'
#' # See the website link above
#' 
#' @export

plotly_ts <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                    mains = "", mainouter="", legendtexts = NA, xat = NA, usely = FALSE, p = NA, ...){
    UseMethod("plotly_ts")
}

#' @export
plotly_ts.data.list <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                              mains = "", mainouter="", legendtexts = NA, xat = NA, usely=TRUE, p=NA, kseq = NA, ...) {
    plot_ts.data.list(object=object, patterns=patterns, xlim = xlim, ylims = ylims, xlab = xlab, ylabs = ylabs,
                      mains = mains, mainouter=mainouter, legendtexts = legendtexts, xat = xat, usely = usely, p = p, kseq=kseq, ...)
}

#' @export
plotly_ts.data.frame <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                              mains = "", mainouter="", legendtexts = NA, xat = NA, usely=TRUE, p=NA, namesdata=NA, ...) {
    plot_ts.data.frame(object=object, patterns=patterns, xlim = xlim, ylims = ylims, xlab = xlab, ylabs = ylabs,
                      mains = mains, mainouter=mainouter, legendtexts = legendtexts, xat = xat, usely = usely, p = p, namesdata=namesdata, ...)
}

## plotly_ts.rls_fit <- function(fit, xlim=NA, kseq=NA, plotit=TRUE){
##     plotly_ts.rls_fit(fit, xlim=xlim, kseq=kseq, plotit=plotit, usely=TRUE)
## }
