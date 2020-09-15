# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?aslt

#' Setting \code{\link{par}()} plotting parameters to a set of default values
#'
#' A simple function, which sets the \code{\link{par}()} plotting parameters to a default set of values.
#'
#' Actually, only really used for setting useful \code{par} values for multiple time series plots with same x-axis.
#' Give \code{tmpl="ts"} and \code{mfrow=c(x,1)}, where x is the number of plots.
#' 
#' @title Setting \code{\link{par}()} plotting parameters
#' @param tmpl The name of the parameter template, give "ts" as default
#' @param mfrow The mfrow for \code{par}.
#' @param ... More parameters for \code{par}.
#' @return Return the original set of parameters, such that they can be reset after plotting.
#' @examples
#'
#' # Make some data
#' D <- data.frame(t=seq(ct("2020-01-01"),ct("2020-01-10"),len=100), x=rnorm(100), y=runif(100))
#' # Remember the currect par values
#' oldpar <- setpar()
#'
#' # Generate two stacked plots with same x-axis
#' setpar("ts", mfrow=c(2,1))
#' plot(D$t, D$x, type="l")
#' plot(D$t, D$y, type="l")
#' # Note xaxt="s" must be set
#' axis.POSIXct(1, D$t, xaxt="s", format="%Y-%m-%d")
#'
#' # Set back the par to the previous
#' par(oldpar)
#' 
#' # In a function, where this is used and a plot is generated,
#' # then do like this in order to automatically reset on exit
#' oldpar <- setpar(mfrow=c(2,1))
#' on.exit(par(oldpar))        
#'
#' @importFrom graphics par
#' @export
setpar <- function(tmpl = "ts", mfrow = c(1,1), ...) {
    # Get par list
    p <- par(no.readonly = TRUE)
    # Templates
    if (tmpl == "ts") {
        par(mfrow = mfrow, oma = c(3, 0, 2, 0), mar = c(0, 4, 1, 0), xaxt = "n", 
            mgp = c(2.2, 0.4, 0), tcl = -0.4, ...)
    }else if (tmpl == "pdf") {
        par(mar = c(4, 4, 1, 1), mgp = c(2.2, 0.7, 0), tcl = -0.4, ...)
    }else{
        stop("Must give tmpl like 'ts' or 'pdf'")
    }
    
    # Replace all the parameters given in prm Get only the ... parameters
    i <- which(!nams(match.call()) %in% nams(match.call(expand.dots = TRUE)))
    if (length(i) > 0) {
        par(...)
        # prm <- as.list(match.call()[i]) p <- list() for(i in 1:length(prm)) { p$new <-
        # eval(prm[[i]]) nams(p)[i] <- nams(prm)[i] } par(p)
    }
    # Only done to remove NOTE in "R CMD check", that R6 package is not used
    if(FALSE){ R6::R6Class("temp", public = list(x=1)) }
    # Set par and return the original par options(warn = (-1)) options(warn = 1)
    invisible(p)
}
