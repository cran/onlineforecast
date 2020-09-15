# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?plot_ts
#?plot_ts.data.list
#?plot_ts.data.frame

#' Plot time series of observations and forecasts (lagged to be aligned in time).
#'
#' Generates time series plots depending on the variables matched by each regular expression given in the \code{patterns} argument.
#' 
#' The forecasts matrices in the \code{data.list} given in \code{object} will be lagged to be aligned in time (i.e. k-step forecasts will be lagged by k).
#'
#' Use the plotly package if argument \code{usely} is TRUE, see \code{\link{plotly_ts}()}.
#'
#' @title Time series plotting
#' @param object A \code{data.list} or \code{data.frame} with observations and forecasts, note diffe
#' @param patterns A character vector with regular expressions, see examples for use.
#' @param xlim The time range as a character of length 2 and form "YYYY-MM-DD" or POSIX. Date to start and end the plot.
#' @param ylims The \code{ylim} for each plot given in a list.
#' @param xlab A character with the label for the x-axis.
#' @param ylabs A character vector with labels for the y-axes.
#' @param mains A character vector with the main for each plot.
#' @param mainouter A character with the main at the top of the plot (can also be added afterwards with \code{title(main, outer=TRUE)}).
#' @param legendtexts A list with the legend texts for each plot (replaces the names of the variables).
#' @param xat POSIXt specifying where the ticks on x-axis should be put.
#' @param usely If TRUE then plotly will be used.
#' @param plotit If FALSE then the plot will not be generated, only data returned.
#' @param p The plot_ts parameters in a list, as generated with the function \code{\link{par_ts}()}.
#' @param ... Parameters passed to \code{\link{par_ts}}, see the list of parameters in \code{?\link{par_ts}}.
#' @seealso
#' \code{\link{par_ts}} for setting plot control parameters.
#'
#' \code{\link{regex}} for regular expressions to select which variables to plot.
#' 
#' @return A list with a data.frame with the data for each plot, if usely=TRUE, then a list of the figures (drawn with print(subplot(L, shareX=TRUE, nrows=length(L), titleY = TRUE))).
#'
#' @examples
#'
#' # Time series plots for \code{data.list}, same as for \code{data.frame} except use of \code{kseq}
#' D <- Dbuilding
#' plot_ts(D, c("heatload","Ta"), kseq=c(1,24))
#' # Make two plots (and set the space for the legend)
#' plot_ts(D, c("heatload","Ta"), kseq=c(1,24), legendspace=11)
#' # Only the Ta observations 
#' plot_ts(D, c("heatload","Taobs$"), kseq=c(1,24), legendspace=11)
#'
#' # Give labels
#' plot_ts(D, c("heatload","Ta"), kseq=c(1,24), xlab="Time", ylabs=c("Heat (kW)","Temperature (C)"))
#' # Mains (see mainsline in par_ts())
#' plot_ts(D, c("heatload","Ta"), kseq=c(1,24), mains=c("Heatload","Temperature"), mainsline=c(-1,-2))
#'
#' # Format of the xaxis (see par_ts())
#' plot_ts(D, c("heatload","Ta"), kseq=c(1,24), xaxisformat="%Y-%m-%d %H:%m")
#'
#' # Return the data, for other plots etc.
#' L <- plot_ts(D, c("heatload","Ta"), kseq=c(1,24))
#' names(L[[1]])
#' names(L[[2]])
#'
#'
#' @rdname plot_ts
#' @export
plot_ts <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                    mains = "", mainouter="", legendtexts = NA, xat = NA, usely = FALSE, plotit = TRUE, p = NA, ...){
    UseMethod("plot_ts")
}



#' @param kseq For \code{class(object)=="data.list"} an integer vector, default = NA. Control which forecast horizons to include in the plots. If NA all the horizons will be included.
#' @rdname plot_ts
#' @export
plot_ts.data.list <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                              mains = "", mainouter="", legendtexts = NA, xat = NA, usely=FALSE, plotit = TRUE, p=NA, kseq = NA, ...) {
    # Take par_ts setup parameters from options if there
    p <- par_ts(fromoptions=TRUE, p=p, ...)
    #
    DL <- object
    # Do a bit of checking
    if(is.null(DL$t)){ stop("No 't' in the data.list.")}
    # Should a subset be taken according to xlim?
    if(!is.na(xlim[1]) | length(xlim) > 1){
            if(is.na(xlim[1])) { xlim[1] <- DL$t[1] }
            if(length(xlim) == 1) { xlim[2] <- as.character(DL$t[length(DL$t)]) }
            DL <- subset(DL, in_range(xlim[1], DL$t, xlim[2]))
        }
    # More checking
    if(length(DL$t) == 0){ stop(pst("No data in the time range. ",xlim[1]," to ",xlim[2]))}
    # set kseq if not specified
    if( is.na(kseq[1]) ){
        tmp <- unique(unlist(lapply(DL, function(x){names(x)})))
        tmp <- tmp[grep("^[k|h][[:digit:]]+$", tmp)]
        if( length(tmp) > 0 ){ kseq <- sort(as.integer(gsub("[k|h]","",tmp))) }
    }
    # Generate a data.frame with the series to be plotted
    X <- lapply_cbind_df(patterns, function(pattern) {
        # Find the variables to plot
        nms <- grep(pattern, names(DL), value = TRUE)
        #
        if(length(nms) == 0){
            warning("No names where found matching the pattern '",pattern,"'")
            tmp <- as.data.frame(matrix(NA,nrow=length(DL$t),ncol=1))
            names(tmp)[1] <- pattern
            return(tmp)
        }else{
            # Go through the names in nms
            do.call("cbind", lapply(nms, function(nm){
                if(is.null(dim(DL[[nm]]))) {
                    # It is a vector, just return it
                    X <- data.frame(DL[[nm]])
                    names(X) <- nm
                    return(X)
                } else {
                    # Its a matrix
                    # Find the columns with 'k' and digits
                    # Note the convention:
                    #   - starting with 'k' it's a forecast for t+k, and must be lagged to sync
                    #   - if it starts with 'h' then it's an observation for the k'th horizon and for
                    helper <- function(prefix){
                        i <- which(nams(DL[[nm]]) %in% pst(prefix,kseq))
                        if(length(i) > 0) {
                            X <- DL[[nm]][ ,i]
                            # Started with k, then it's forecasts and must be lagged to sync
                            if( prefix == "k" ){
                                ks <- as.integer(gsub("k","",nams(DL[[nm]])[i]))
                                X <- lagdf(X, lagseq=ks)
                            }
                            # Fix if it is a vector
                            if(is.null(dim(X))) {
                                X <- as.data.frame(X)
                                names(X) <- nams(DL[[nm]])[i]
                            }
                            nams(X) <- pst(nm, "_", nams(X))
                            return(X)
                        }else{
                            return(NULL)
                        }
                    }
                    X <- helper("k")
                    if(is.null(X[1])){
                        X <- helper("h")
                    }
                    if(is.null(X[1])){
                        # Not started with "k" or "h": Just take all columns
                        X <- as.data.frame(DL[[nm]])
                        names(X) <- pst(nm,"_",names(X))
                    }
                    return(X)
                }
            }))
        }
    })
    # 
    if(any(duplicated(nams(X)))){
        X <- X[ ,unique(nams(X))]
    }
    # Add "t" for the x-axis
    X$t <- DL$t
    # Since we added "_k" or "_h" to the matrix variables, we have to
    # strip it off, and pass on as the names to search for with patterns
    namesdata <- unlist(getse(strsplit(nams(X), "_k|_h"), 1))
    # Use the plot_ts function which takes the data.frame
    plot_ts.data.frame(X, patterns, ylims = ylims, xlab = xlab, ylabs = ylabs, mains = mains, mainouter = mainouter,
                       legendtexts = legendtexts, xat = xat, usely=usely, plotit = plotit, p=p, namesdata=namesdata, ...)
}


# Plot all with prefix
#' @param namesdata For \code{class(object)=="data.frame"} a character vector. Names of columns in object to be searched in, instead of \code{names(object)}.
#' @rdname plot_ts
#' @importFrom graphics par title
#' @export
plot_ts.data.frame <- function(object, patterns=".*", xlim = NA, ylims = NA, xlab = "", ylabs = NA,
                               mains = NA, mainouter="", legendtexts = NA, xat = NA, usely=FALSE, plotit = TRUE, p = NA, namesdata=NA, ...) {
    # Take par_ts setup parameters from options if there
    p <- par_ts(fromoptions=TRUE, p=p, ...)
    #
    data <- object
    # Do a bit of checking
    if(nrow(data) == 0){ stop("No rows in the data.frame.")}
    if(is.null(data[ ,p$xnm])){ warning("No 't' or xnm found. If time is not in 't', then specify it in xnm (either as argument or in options(\"par_ts\").")}
    #
    if(!is.null(data[ ,p$xnm])){
        if(is.na(xlim[1])) { xlim[1] <- data[1,p$xnm] }
        if(length(xlim)==1) { xlim[2] <- data[nrow(data),p$xnm] }
        data <- data[in_range(xlim[1], data[ ,p$xnm], xlim[2]), ]
    }
    # More checking
    if(nrow(data) == 0){ stop(pst("No data in the time range. ",xlim[1]," to ",xlim[2]))}
    # Extend all individual plots vars, if not set
    if(is.na(mains[1])){ mains <- rep(NA,length(patterns)) }
    mainsline <- p$mainsline
    if(is.na(mainsline[1])){ mainsline <- rep(NA,length(patterns)) }
    if(is.na(ylims[1])){ ylims  <- as.list(rep(NA,length(patterns))) }
    if(is.na(ylabs[1])){ ylabs  <- rep(NA,length(patterns)) }
    if(is.na(legendtexts[1])){ legendtexts <- as.list(rep(NA,length(patterns))) }
    #
    if(usely){
        # with plotly
        if(!requireNamespace("plotly", quietly = TRUE)){
#            loadNamespace("plotly")
#        }else{
            stop("The plotly package must be installed.")
        }
        #
        Liseq <- list()
        for(ii in 1:length(patterns)) {
            Liseq[[ii]] <- plot_ts_iseq(data, patterns[ii], p$xnm, namesdata)
        }
        nlines <- length(unlist(Liseq))
        # Longest name
        maxchar <- max(nchar(names(data)[unique(unlist(Liseq))]))
        #colormap <- p$colorramp(nlines)
        #
        L <- list()
        # This is hacky, but take the operator from the namespce directly (cannot plotly::%>% below. And don't want to plotly in imports, since it's big and don't want to force installation)
        '%>%' <- plotly::'%>%'
        #
        for(ii in 1:length(patterns)) {
            iseq <- Liseq[[ii]]
            #
            fig <- plotly::plot_ly(x=data[ ,p$xnm])
            for(i in 1:length(iseq)){
                fig <- fig %>% plotly::add_lines(y = data[ ,iseq[i]],
                                                         name = names(data)[iseq[i]],
                                                         # color = colormap[iii]
                                                         )#, legendgroup = paste0('group',ii))
            }
            if(ii < length(patterns)){
                # Add empty to make legend gap
                fig <- fig %>% plotly::add_lines(y = rep("NA",nrow(data)), name=strrep("-",maxchar), line=list(color="white"))
            }
            # Add ylabs?
            if(!is.na(ylabs)[1]){
                fig <- fig %>% plotly::layout(yaxis=list(title=ylabs[ii]))
            }
            fig <- fig %>% plotly::layout(xaxis=list(title=xlab))
            # Keep it
            L[[ii]] <- fig
        }
        # Center legend
        L[[ii]] <- L[[ii]] %>% plotly::layout(legend = list(x = 100, y = 0.5))
        # Draw it
        if(plotit){
            print(plotly::subplot(L, shareX=TRUE, nrows=length(L), titleY = TRUE))
        }
        # Return if needed
        invisible(L)
    }else{
        #
        oldpar <- setpar("ts", mfrow=c(length(patterns),1), cex=p$cex)
        par(xpd=TRUE, mar = c(0, 4, 0.5, p$legendspace))
        on.exit(par(oldpar))
        #
        L <- lapply(1:length(patterns), function(i){
            df <- plot_ts_series(data, patterns[i], iplot=i, ylim=ylims[[i]], xlab=xlab, legendtext = legendtexts[[i]],
                           main=mains[i], mainline=mainsline[i], xat = xat, plotit=plotit, p=p, namesdata=namesdata,
                           xaxis=(i==length(patterns)), ...)
            title(mainouter, outer=TRUE)
            if (!is.na(ylabs[1])){
                title(ylab = ylabs[i], yaxt = "s")
            }
            return(df)
        })
        invisible(L)
    }
}

#' @rdname plot_ts
#' @importFrom graphics par title
#' @export
plot_ts.matrix <- plot_ts.data.frame


plot_ts_iseq <- function(data, pattern, xnm, namesdata){
    iseq <- integer(0)
    # Use these names when finding columns to plot
    if(is.na(namesdata)[1]){
        nms <- nams(data)
    }else{
        nms <- namesdata
    }
    # Find indexes of patterns in data
    # Do the for loop, to secure the order of the patterns between "|"s
    for (pf in strsplit(pattern, "\\|")[[1]]) {
        iseq <- c(iseq, grep(pf, nms))
    }
    # Only take unique (keeps the order)
    iseq <- unique(iseq) 
    #
    # Remove p$xnm if in nms
    iseq <- iseq[!nms[iseq] == xnm]
    #
    return(iseq)
}


# Plot all columns found with regex pattern
#' @importFrom graphics plot lines axis title axis.POSIXct mtext par legend
plot_ts_series <- function(data, pattern, iplot = 1,
                           ylim = NA, xlab = "", main = "", mainline = -1.2, legendtext = NA, xat = NA, plotit = TRUE, p = NA, namesdata = NA, xaxis = TRUE, ...) {
    #
    # Take par_ts setup parameters from options or defaults
    p <- par_ts(fromoptions=TRUE, p=p, ...)
    #
    iseq <- plot_ts_iseq(data, pattern, p$xnm, namesdata)
    # Check if p$xnm is in the data
    if (any(names(data) == p$xnm)) {
        x <- data[, p$xnm]
    } else {
        x <- 1:nrow(data)
    }
    #
    if(plotit){
        #
        xlim <- c(min(x)+diff(range(x))*0.02, max(x))
        if(any(is.na(xlim))){ stop("Could not calculate range of x, probably there is NA in t!") }
        #
        if (all(is.na(iseq)) | length(iseq)==0){
            # No series to plot
            legendtext <- pst(pattern," not found")
            colormap <- 1
            ylim <- c(0,1)
            yat <- NA
        }else{
            # Limits on y-axis
            if (is.na(ylim[1])) {
                # For some weird reason range doesn't work with a data.frame of logicals, it works only on a vector of logicals
                if(all(sapply(iseq, function(i){ is.logical(data[, i]) }))){
                    # All are logicals
                    ylim <- c(0,1)
                }else{
                    ylim <- range(data[, iseq], na.rm = TRUE)
                    if(any(is.na(ylim))){
                        legendtext <- pst(pattern," all NA")
                        colormap <- 1
                        ylim <- c(0,1)
                        yat <- NA
                    }
                }
            }
            #
            colormap <- p$colorramp(length(iseq))
            #
            # EXTEND THE YLIM: to make room for multiple plots
            ylim <- ylim + c(-1,1) * diff(ylim) * p$ylimextend
            # for axis
            ylimmod <- ylim + c(-1,1) * diff(ylim) * p$yaxisextend
            #
            # Make the legend text
            if (p$legendrangeshow & is.na(legendtext[1])){
                rngtext <- do.call("rbind", lapply(1:length(iseq), function(i) {
                    tmp <- sapply(range(data[, iseq[i]], na.rm = TRUE), function(x){
                        if( x <= 10 ){ return(signif(x, digits = 2)) }else{ return(round(x)) } })
                    gsub("\\s+"," ",paste(tmp, collapse = " to "), perl=TRUE)
                }))
                legendtext <- paste0(nams(data)[iseq], ": ", rngtext)
            }else{
                legendtext <- paste0(nams(data)[iseq])
            }
        }
        #
        plot(x, x, type = "n", xlim = xlim, ylim = ylim, xaxs="r", yaxt = "n", bty = "n",
             xlab = "", ylab = "")
        # For grid
        xb <- c(xlim[1]-0.04*diff(xlim), xlim[2]+0.01*diff(xlim))
        # yb <- c(ylim[1]-0.04*diff(ylim), ylim[2]+0.01*diff(ylim))
        if(all(sapply(data[ ,iseq], class) == "logical")){
            # Its all logical
            yb <- c(0,1)
        }else{
            yb <- range(data[ ,iseq], na.rm=TRUE) #c(ylim[1]-0.04*diff(ylim), max(data[ ,iseq],na.rm=TRUE))#, ylim[2]+0.01*diff(ylim))
        }
        # BACKGROUND
        # polygon(c(xb[1],xb,rev(xb)), c(yb,rev(yb),yb[1]), col="grey95", lty=0)
        # GRID
        if(is.na(xat)){
            xat <- pretty(x)
            irm <- which(xat < min(x) | xat > max(x))
            if(length(irm)){ xat <- xat[-irm] }
        }
        if(!"yat" %in% ls()){
            # Where to put y grid and labels
            yat <- pretty(ylimmod, 3)
            yat <- yat[(ylim[1]-0.04*diff(ylim)) < yat]
            yat <- yat[yat <= ylim[2]]
            sapply(yat, function(y){ lines(xb,c(y,y), col="lightgrey", lty="dotted") })
            axis(2, yat, lwd=0, lwd.ticks=1)
            lines(rep(xb[1],2), range(yat))
        }
        # horizontal grid
        ygrid <- c(ylim[1]-0.04*diff(ylim), max(yb,yat))
        sapply(xat, function(x){ lines(c(x,x),ygrid,col="lightgrey",lty="dotted") })
        #
        # PLOT LINES
        if (!all(is.na(iseq))){
            for (i in 1:length(iseq)) {
                p$plotfun(x, data[, iseq[i]], col = colormap[i])
            }
        }
        title(main = main, line=mainline)
        # 
        # Make the xaxis
        if( xaxis ){
            if (any(nams(data) == p$xnm)) {
                if (class(data[ ,p$xnm])[1] != "POSIXct") {
                    axis(1, data[ ,p$xnm], xaxt = "s")
                } else {
                    # makes too few ticks: axis.POSIXct(1, data[ ,p$xnm], format = xaxisformat, xaxt = "s")
                    if(is.na(xat[1])){ xat <- pretty(data[ ,p$xnm]) }
                    # Format, per default NA, so make it here
                    xaxisformat <- p$xaxisformat
                    if(is.na(xaxisformat)){
                        if( all(as.numeric(xat,unit="secs") %% (24*3600) == 0) ){
                            xaxisformat <- "%Y-%m-%d"
                        }else{
                            xaxisformat <- "%Y-%m-%d %H:%M"
                        }
                    }
                    axis.POSIXct(1, data[ ,p$xnm], at = xat, format = xaxisformat, xaxt = "s", lwd=1, lwd.ticks=1)
                }
            } else {
                axis(1, 1:nrow(data), xaxt = "s", lwd=0, lwd.ticks=1)
            }
            mtext(xlab, 1, line=par()$mgp[1]-0.5)
        }
        #
        legend(x=xlim[2]+2*0.01*diff(xlim), y=ylim[2], legend=legendtext, lty = 1, col = colormap, cex = p$legendcex, bg="white", box.lwd=0.5)
    }
    #
    invisible(cbind(t=x, data[, iseq]))
}



#' Plot forecasts, residuals, cumulated residuals and RLS coefficients
#'
#' A useful plot for residual analysis and model validation of an RLS fitted forecast model.
#'
#' All parameters, except those described below, are simply passed to \code{\link{plot_ts}()}.
#' 
#' @title Plots for an rls_fit.
#' @param fit An \code{rls_fit}.
#' @param patterns See \code{\link{plot_ts}}. The default pattern finds the generated series in the function, '!!RLSinputs!!' will be replaced with the names of the RLS inputs (regression stage inputs).
#' @return The plotted data in a \code{data.list}.
#'
#' @seealso \code{\link{plot_ts}}.
#' @examples
#'
#' # Fit a model (see vignette 'setup-and-use-model'
#' D <- Dbuilding
#' D$scoreperiod <- in_range("2010-12-22", D$t)
#' model <- forecastmodel$new()
#' model$output = "heatload"
#' model$add_inputs(Ta = "Ta",
#'                  mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.9)")
#' model$kseq <- c(3,18)
#' fit1 <- rls_fit(NA, model, D, returnanalysis = TRUE)
#'
#' # Plot it
#' plot_ts(fit1)
#'
#' # Return the data
#' Dplot <- plot_ts(fit1)
#'
#' # The RLS coefficients are now in a nice format
#' head(Dplot$mu)
#'
#' @rdname plot_ts
#' @export
plot_ts.rls_fit <- function(object, patterns = c("^y$|^Yhat$","^Residuals$","CumAbsResiduals$",pst("^",names(fit$Lfitval[[1]]),"$")),
                          xlim = NA, ylims = NA, xlab = "", ylabs = NA, mains = "", mainouter="", legendtexts = NA,
                          xat = NA, usely=FALSE, plotit=TRUE, p=NA, kseq = NA, ...){
    fit <- object
    # Calculate the residuals
    Residuals <- residuals(fit)
    # Prepares a data.list for the plot
    if(is.na(xlim[1])){
        if(!("scoreperiod" %in% names(fit$data))){
            isubset <- c(1,length(fit$data$t))
        }else{
            # Default is to plot the scoreperiod if there
            isubset <- which(fit$data$scoreperiod)
        }
        isubset <- min(isubset,na.rm=TRUE):max(isubset,na.rm=TRUE)#1:length(fit$data$t)
    }else{
        isubset <- in_range(xlim[1], fit$data$t, xlim[2])
    }
    if(is.na(kseq[1])){
        kseq <- fit$model$kseq
    }
    #
    CumAbsResiduals <- lapply_cbind_df(kseq, function(k){
        tmp <- abs(Residuals[isubset,pst("h",k)])
        tmp[is.na(tmp)] <- 0
        cumsum(tmp)
    })
    names(CumAbsResiduals) <- pst("h",kseq)
    #
    # Convert the parameter estimates
    nmsinput <- names(fit$Lfitval[[1]])
    tmp <- lapply(1:length(nmsinput), function(i){
        # Name of the input
        nm <- names(fit$Lfitval[[1]])[i]
        # Take the fitues each horizon for the input
        ik <- which(names(fit$Lfitval) %in% pst("k",kseq))
        X <- lapply_cbind_df(ik, function(ii){
            fit$Lfitval[[ii]][isubset,nm]
        })
        names(X) <- gsub("k","h",names(fit$Lfitval)[ik])
        return(X)
    })
    names(tmp) <- nmsinput
    #
    data <- list(y=fit$data[[fit$model$output]][isubset], Yhat=fit$Yhat[isubset,pst("k",kseq)], Residuals=Residuals[isubset,pst("h",kseq)], CumAbsResiduals=CumAbsResiduals)
    data <- c(data,tmp)
    data$t <- fit$data$t[isubset]
    class(data) <- "data.list"
    if(plotit){
        #
        if(is.na(ylabs[1])){
            ylabs <- c("Model output","Residuals","Cum. residuals",pst("Coef: ",nmsinput))
        }
        # The input names
        #patterns[patterns == "!!RLSinputs!!"] <- pst("^",nmsinput,"$"))
        # Make a plot of the RLS coefficients for each horizon
        plot_ts(data, patterns, xlim = xlim, ylims = ylims, xlab = xlab, ylabs = ylabs,
                mains = mains, mainouter=mainouter, legendtexts = legendtexts, xat = xat, usely=usely, p=p, kseq = kseq, ...)
    }
    # Return the data
    invisible(data)
}
