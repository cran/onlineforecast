#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?stairs


#' Plotting steps with time point at end of interval
#'
#' It's easy to plot stairs with \code{plot(x,y,type="s")}, however that makes the steps forward from \code{x}, for time series this works if the time points are at the beginning of the intervals.
#'
#' Often with time series the time points are in the end of the intervals, so the steps should go backaward, this is achieved with this function.
#' 
#' @title Plotting stairs with time point at end of interval.
#' @param x x values for plot.
#' @param y y values for plot.
#' @param type if 'b' then include points.
#' @param preline if TRUE, then a line backwards from the first point is added.
#' @param pch Passed to \code{points()}.
#' @param ... Passed to \code{lines()} and \code{points()} when they are called in the function.
#' @examples
#'
#' # Usual stairs plot has steps forward from x
#' x <- rnorm(10)
#' plot(1:10, x, type="s")
#'
#' # Stairs with step backward from x
#' plot(1:10, x, type="n")
#' stairs(1:10, x)
#'
#' # Use for time series plotting
#' plot_ts(Dbuilding, "heatload", c("2010-12-15","2010-12-16"), plotfun=stairs)
#'
#' # Set it globally for all plot_ts
#' p <- par_ts()
#' p$plotfun <- stairs
#' options(par_ts=p)
#' plot_ts(Dbuilding, "heatload", c("2010-12-15","2010-12-16"))
#'
#' # Modify it to only lines
#' plot_ts(Dbuilding, "heatload", c("2010-12-15","2010-12-16"),
#'         plotfun=function(x,y,...){stairs(x,y, type="l")})
#'
#' @importFrom graphics lines points
#' @export
stairs <- function(x, y, type="b", preline=FALSE, pch=19, ...)
{
    xp <- rep(x,each=2)
    yp <- rep(y,each=2)

    xp <- c(xp[1]-(xp[3]-xp[1]), xp)
    yp <- c(yp,yp[length(yp)])
    if(!preline){
        yp[1] <- NA
    }
    #
    lines(xp, yp, ...)
    if(type == "b"){
        points(x, y, pch=pch, ...)
    }
}
