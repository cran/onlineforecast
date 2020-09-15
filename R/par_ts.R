# # Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?par_ts

#' Set parameters for \code{\link{plot_ts}()} globally
#'
#' Often in a report some plot parameters must be set for all plots, which is done with \code{\link{par}()}.
#'
#' The parameters which are general for \code{\link{plot_ts}()} can be set and saved in \code{\link{options}()},
#' and they will then be applied as default in all calls to plot_ts(). See the examples how to do this.
#'
#' If any of these parameters are given to \code{\link{plot_ts}()}, then it will be used over the default.
#'
#' @title Set parameters for \code{\link{plot_ts}()}
#' @param fromoptions logical: Read the list of parameters set in \code{\link{options}("par_ts")$par_ts}, then the additional parameters set in \code{...} are replaced before the list is returned.
#' @param p List of the parameters, as returned by the function itself. If given, the additional parameters set in \code{...} are replaced before the list is returned.
#' @param ... any of the following parameters can be set replacing the default values:
#' 
#' \code{xnm} "t": The name of the time
#'
#' \code{legendspace} 10:
#' Horizontal space for the lengend in character spaces
#'
#' \code{legendcex} 1: Scaling of the legend
#' 
#' \code{legendrangeshow} TRUE: Include the range for each variable in the legend
#' 
#' \code{ylimextend} c(lower,upper): Extend the ylim for each plot with a proportion, seperately for the lower and upper limit
#' 
#' \code{yaxisextend} c(lower,upper): Extend the yaxis for each plot with a proportion, seperately for the lower and upper limit
#' 
#' \code{mainsline} (numeric): with the \code{line} for the main in the plots.
#' 
#' \code{cex} (numeric): The cex to use for the \code{plot_ts} plots.
#' 
#' \code{plotfun}: The function used for plotting, as default \code{lines}.
#' 
#' \code{xaxisformat} (character): The format of the xaxis, see \code{\link{strptime}()}.
#' 
#' \code{colorramp} colorRampPalette: The colorramp used for setting multiple colors in each plot
#'
#' @return A list of the parameters above, which can be set globally (see examples) or passed to \code{\link{plot_ts}}.
#' 
#' @examples
#'
#' # Data for plots
#' D <- subset(Dbuilding, 1:192)
#'
#' # See the parameters which can be set
#' p <- par_ts()
#' names(p)
#' p$xnm
#'
#' # Using the default values
#' plot_ts(D, c("heatload","Ta"), kseq=1:24)
#'
#' # Set the parameters directly
#' plot_ts(D, c("heatload","Ta"), kseq=1:24, legendcex=0.8, legendspace=8)
#'
#' # Set parameters to be given in a list
#' p <- par_ts()
#' p$legendcex <- 0.8
#' p$legendspace <- 8
#'
#' # Use those parameters
#' plot_ts(D, c("heatload","Ta"), kseq=1:24, p=p)
#'
#' # Set globally (if not set specifed the default values will be used)
#' options(par_ts=p)
#'
#' # Now the global parameters will be used
#' plot_ts(D, c("heatload","Ta"), kseq=1:24)
#'
#' # Still providing a parameter directly it will used, e.g. change the plotting function
#' plot_ts(D, c("heatload","Ta"), kseq=1:24, plotfun=points)
#'
#' # Control more precisely the plotting function
#' plot_ts(D, c("heatload","Ta"), kseq=1:24, plotfun=function(x, ...){ points(x, type="b", ...)})
#'
#' # Another colorramp function
#' p$colorramp <- rainbow
#' options(par_ts=p)
#' plot_ts(D, c("heatload","Ta"), kseq=1:24)
#'
#' @importFrom graphics lines
#' @importFrom grDevices colorRampPalette
#' @export
par_ts <- function(fromoptions=FALSE, p=NA, ...){
    # Take the values in options= if they are there
    if(is.na(p)[1]){
        if(fromoptions & !is.null(options("par_ts")$par_ts)){
            p <- options("par_ts")$par_ts
        }else{
            # Return a list with the default values
            p <- list()
            # Name of the variable for the x axis
            p$xnm <- "t"
            # Legend:
            p$legendspace <- 10       # Space for the legend
            p$legendcex <- 1          # Cex for the legend
            p$legendrangeshow <- TRUE # Add the range to the legendtext
            #
            p$ylimextend <- c(0,0.1)
            #
            p$yaxisextend <- c(0,-0.25)
            # Default is NA, it will be set depeding on the t in plot_ts_series
            p$xaxisformat <- NA
            #
            p$cex <- 1
            #
            p$mainsline <- -1.2
            # The default plot function, overwrite it with another function to change it
            p$plotfun <- lines
            # Color function, can be replaced with in-built or others, e.g. rainbow()
            p$colorramp <- colorRampPalette(c("black","cyan","purple","blue","red","green"))
        }
    }
    # Replace all the parameters given in ...
    args <- list(...)
    nms <- nams(p)[nams(p) %in% nams(args)]
    if (length(nms) > 0) {
        for(nm in nms){
            # If it is a function, the get it parent environment
            p[[nm]] <- args[[nm]]
        }
    }
    return(p)
}
