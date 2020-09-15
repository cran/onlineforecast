# Do this in a separate tmp.R file to check the documentation
# library(devtools)
# document()
# load_all(as.package("../../onlineforecast"))
# ?as.data.list
# ?data.list
#?as.data.list.data.frame


#' Make a data.list of the vectors and data.frames given.
#'
#' See the vignette 'setup-data' on how a data.list must be setup.
#' 
#' It's simply a list of class \code{data.list} holding:
#'   - vector \code{t}
#'   - vector(s) of observations
#'   - data.frames (or matrices) of forecast inputs
#' 
#' 
#' @title Make a data.list
#' @param ... Should hold: time t, observations as vectors and forecasts as data.frames
#' @return a data.list.
#' @examples
#' # Put together a data.list
#' # The time vector
#' time <- seq(ct("2019-01-01"),ct("2019-01-02"),by=3600)
#' # Observations time series (as vector)
#' xobs <- rnorm(length(time))
#' # Forecast input as data.frame
#' X <- data.frame(matrix(rnorm(length(time)*3), ncol=3))
#' names(X) <- pst("k",1:3)
#' 
#' D <- data.list(t=time, xobs=xobs, X=X)
#'
#' # Check it
#' check(D)
#' 
#' @export
data.list <- function(...) {
    structure(list(...), class = "data.list")
}


#' Take a subset of a data.list.
#'
#' Different arguments can be given to select the subset. See the examples.
#' 
#' @title Take a subset of a data.list.
#' @param x The data.list to take a subset of.
#' @param subset Given as the integer indexes or a logical vector, or alternatively \code{c(tstart,tend)}, where tstart and tend are either as POSIX or characters.
#' @param nms The names of the variables in \code{x} to be included.
#' @param kseq The k horizons of forecasts to be included.
#' @param lagforecasts Should the forecasts be lagged k steps (thus useful for plotting etc.).
#' @param pattern Regex pattern applied to select the variables in x to be included.
#' @param ... Not implemented.
#' @return a data.list with the subset.
#' @examples
#' # Use the data.list with building heat load 
#' D <- Dbuilding
#' # Take a subset for the example
#' D <- subset(D, 1:10, nms=c("t","Taobs","Ta","Iobs","I"), kseq=1:3)
#' 
#' # Take subset index 2:4
#' subset(D, 2:4)
#' 
#' # Take subset for a period
#' subset(D, c("2010-12-15 02:00","2010-12-15 04:00"))
#' 
#' # Cannot request a variable not there
#' try(subset(D, nms=c("x","Ta")))
#' 
#' # Take specific horizons
#' subset(D, nms=c("I","Ta"), kseq = 1:2)
#' subset(D, nms=c("I","Ta"), kseq = 1)
#' 
#' # Lag the forecasts such that they are aligned in time with observations
#' subset(D, nms=c("Taobs","Ta"), kseq = 2:3, lagforecasts = TRUE)
#' 
#' # The order follows the order in nms
#' subset(D, nms=c("Ta","I"), kseq = 2)
#' 
#' # Return variables mathing a regex
#' subset(D, kseq=2, pattern="^I")
#' 
#' # Take data for Ta and lag the forecasts (good for plotting and fitting a model)
#' X <- subset(Dbuilding, 1:1000, pattern="^Ta", kseq = 10, lagforecasts = TRUE)
#' 
#' # A scatter plot between the forecast and the observations
#' # (try lagforecasts = FALSE and see the difference)
#' plot(X$Ta$k10, X$Taobs)
#'
#' # Fit a model for the 10-step horizon
#' abline(lm(Taobs ~ Ta.k10, X), col=2)
#'
#' @export
subset.data.list <- function(x, subset = NA, nms = NA, kseq = NA, lagforecasts = FALSE, pattern = NA, ...) {
    D <- x
    # --------------------------------
    # Set nms if needed (find the columns to take)
    if(is.na(nms[1])){
        nms <- names(D)
    }
    # If a pattern is given then find the columns
    if(!is.na(pattern[1])){
        # If the pattern has an or "|", then split on it to get the right order of the names
        nms <- unlist(sapply(strsplit(pattern, "\\|")[[1]], function(pat){
            grep(pat, names(D), value=TRUE)
        }))
    }
    # --------------------------------
    # Input checks
    # Check if all variables are in nms
    if(!all(nms %in% names(D))){ stop(pst("The variable ",nms[nms %in% names(D)]," is not in D"))}
    #
    if(!is.na(kseq)[1]){
        lapply(1:length(nms), function(i){
            X <- D[[nms[i]]]
            if(class(X)[1] == "data.frame" ){
                # Check if holds forecasts by checking if any name is "kxx"
                if(length(grep("^k[[:digit:]]+$", names(X))) > 0){
                    # If it holds forecasts, check that they are all there
                    if( !all(pst("k",kseq) %in% names(X)) ){
                        warning(pst("The variable ",nms[i]," contain ",pst(names(X),collapse=",")," hence doesn't contain all k in kseq = ",pst(kseq,collapse=",")))
                    }
                }
            }
        })
    }
    # --------------------------------
    # If subset is NA then set it
    if(is.na(subset[1])){
        if(is.null(dim(D[[1]]))){
            subset <- 1:length(D[[1]])
        }else{
            subset <- 1:dim(D[[1]])[1]
        }
    }else if(length(subset) == 2){
        if(any(class(subset) %in% c("character","POSIXlt","POSIXct","POSIXt"))){
            # Start and end of a period is given
            subset <- in_range(subset[1], D$t, subset[2])
        }
    }else{
        # Check if a non-meaningful subset is given
        if(any(class(subset) == "character")){
            stop("subset cannot be a character, except if it is of length 2 and can be converted in a POSIX, e.g. subset=c('2020-01-01','2020-01-10'. ")
        }
    }
    # Take all horizons k?
    if(is.na(kseq[1])){
        val <- lapply(D[nms], function(X) {
            if (any(class(X) == "data.frame")) {
                return(X[subset, , drop=FALSE]) # drop = FALSE needed in case data frame only has 1 column, otherwise this does not return a data frame
            } else {
                return(X[subset])
            }
        })
    }else{
        # Multiple horizons (hence length(kseq) > 1)
        # Take the specified horizons
        val <- lapply(D[nms], function(X) {
            if (any(class(X) == "data.frame")) {
                # Check if holds forecasts by checking if any name is "kxx"
                if(length(grep("^k[[:digit:]]+$", names(X))) > 0){
                    return(X[subset,pst("k",kseq), drop=FALSE])
                }else{
                    return(X[subset, , drop=FALSE])
                }
            } else {
                return(X[subset])
            }
        })
    }
    # Lag the forecasts k if specified
    if(lagforecasts){
        val <- lapply(val, function(X){
            if(any(class(X) == "data.frame") & length(grep("^k[[:digit:]]+$",names(X))) > 0) {
                return(lagdf.data.frame(X, lagseq="+k"))
            }else{
                return(X)
            }
        })
    }
    class(val) <- "data.list"
    return(val)
}


#' Converts a data.list to a data.frame.
#'
#' The forecasts in the data.list will result in columns named \code{varname.kxx} in the data.frame.
#' 
#' @title Convert to data.frame
#' @param x The data.list to be converted.
#' @param row.names Not used.
#' @param optional Not used.
#' @param ... Not used.
#' @return A data.frame
#' @examples
#'
#' #' # Use the data.list with building heat load 
#' D <- Dbuilding
#' # Take a subset
#' D <- subset(D, 1:5, nms=c("t","Taobs","Ta","Iobs","I"), kseq=1:3)
#'
#' # Convert to a data.frame, note the names of the forecasts are appended .kxx (i.e. for Ta and I)
#' as.data.frame(D)
#'
#' @export
as.data.frame.data.list <- function(x, row.names=NULL, optional=FALSE, ...){
    # Then convert into a data.frame
    val <- do.call("cbind", x)
    if(class(val) == "matrix"){
        val <- as.data.frame(val)
    }
    # Fix names of data.frames (i.e. forecasts, their names are now "kxx", but should be X.kxx)
    i <- grep("^k[[:digit:]]+$", names(val))
    if(length(i) > 0){
        names(val)[i] <- pst(names(x)[i],".",names(val)[i])
    }
    return(val)
}


#' Generate a pairs plot for the vectors in the data.list.
#'
#' A very useful plot for checking what is in the forecasts, how they are synced and match the observations.
#' 
#' @title Generation of pairs plot for a data.list.
#' @param x The data.list from which to plot.
#' @param subset The subset to be included. Passed to \code{\link{subset.data.list}()}.
#' @param nms The names of the variables to be included. Passed to \code{\link{subset.data.list}()}.
#' @param kseq The horizons to be included. Passed to \code{\link{subset.data.list}()}.
#' @param lagforecasts Lag the forecasts such that they are synced with obervations. Passed to \code{\link{subset.data.list}()}.
#' @param pattern Regex pattern to select the included variables. Passed to \code{\link{subset.data.list}()}.
#' @param lower.panel Passed to \code{\link{pairs}()}.
#' @param panel Passed to \code{\link{pairs}()}.
#' @param pch Passed to \code{\link{pairs}()}.
#' @param cex Passed to \code{\link{pairs}()}.
#' @param ... Passed to \code{\link{pairs}()}.
#' @examples
#' # Take a subset for the example
#' D <- subset(Dbuilding, c("2010-12-15","2011-01-15"), pattern="^Ta|^I", kseq=1:3)
#' pairs(D)
#'
#' # If the forecasts and the observations are not aligned in time,
#' # which is easy to see by comparing to the previous plot.
#' pairs(D, lagforecasts=FALSE)
#' # Especially for the solar I syncronization is really important!
#' # Hence if the forecasts were not synced properly, then it can be detected using this type of plot.
#'
#' # Alternatively, lag when taking the subset
#' D <- subset(Dbuilding, c("2010-12-15","2011-01-15"), pattern="^Ta|^I", kseq=1:3, lagforecasts=TRUE)
#' pairs(D, lagforecasts=FALSE)
#'
#' @importFrom graphics panel.smooth pairs
#' @export
pairs.data.list <- function(x, subset = NA, nms = NA, kseq = NA, lagforecasts = TRUE, pattern = NA, lower.panel=NULL, panel=panel.smooth, pch=20, cex=0.7, ...){
    # First take the subset
    X <- as.data.frame(subset(x, subset = subset, nms = nms, kseq = kseq, lagforecasts = lagforecasts, pattern = pattern))
    #
    pairs(X, lower.panel=lower.panel, panel=panel, pch=pch, cex=cex, ...)
}



#' Checking the object for appropriate form. 
#'
#' Prints on table form the result of the check.
#' 
#' @title Checking the object for appropriate form. 
#' @param object The object to be checked.
#' @return The tables generated.
#'
#' # Check a data.list (see \code{?\link{check.data.list}})
#' check(Dbuilding)
#' 
#' @export
check <- function(object){
    UseMethod("check")
}

#' Checking the data.list for appropriate form. 
#'
#' Prints a check of the time vector t, which must have equidistant time points and no NAs.
#'
#' Then the results of checking vectors (observations):
#'   - ok: A 'V' indicates a successful check
#'   - maxNAs: Proportion of NAs
#'   - length: printed if not the same as the 't' vector
#'   - class: the class
#' 
#' Then the results of checking data.frames and matrices (forecasts):
#'   - ok: a 'V' indicates a successful check
#'   - maxNAs: the proportion of NAs for the horizon (i.e. column) with the highest proportion of NAs
#'   - meanNAs: the proportion of NAs of the entire data.frame
#'   - nrow: printed if not the same as the 't' vector length
#'   - colnames: columns must be names 'kxx', where 'xx' is the horizon
#'   - sameclass: 'X' if not all columns are the same class
#'   - class: prints the class of the columns if they are all the same
#' 
#' @title Checking the data.list for appropriate form. 
#' @param object The object to be checked.
#' @return The tables generated.
#'
#' # Check a data.list (see \code{?\link{check.data.list}})
#' check(Dbuilding)
#'
#' # Vector with observations not same length as t
#' D <- Dbuilding
#' D$heatload <- D$heatload[1:10]
#' check(D)
#'
#' # Some NAs in k1 forecast
#' D <- Dbuilding
#' D$Ta$k1[1:1500] <- NA
#' check(D)
#'
#' # Wrong column names
#' names(D$Ta)
#'
#' @export
check.data.list <- function(object){
    # Check if how the data.list is setup and report potential issues
    D <- object
    if(!"t" %in% names(D)){ stop("'t' is missing in the data.list: It must be a vector of equidistant time points (can be an integer, but preferably POSIXct class with tz 'GMT' or 'UTC'.)") }

    if(length(unique(diff(D$t))) != 1){ stop("'t' is not equidistant and have no NA values")}
    message("\nTime t is fine: Length ",length(D$t),"\n")

    # Which is data.frame or matrix?
    dfOrMat <- sapply(D, function(x){ (class(x) %in% c("matrix","data.frame"))[1] })
    # Vectors check
    vecchecks <- c("ok","NAs","length","class")
    vecseq <- which(!dfOrMat & names(dfOrMat) != "t")
    Observations <- data.frame(matrix("", nrow=length(vecseq), ncol=length(vecchecks), dimnames=list(names(vecseq),vecchecks)), stringsAsFactors=FALSE)
    Observations$ok <- "V"
    #
    for(i in 1:length(vecseq)){
        #
        nm <- names(vecseq)[i]
        # NAs
        NAs <- round(max(sum(is.na(D[nm])) / length(D[nm])))
        Observations$NAs[i] <- pst(NAs,"%")
        # Check the length
        if(length(D[[nm]]) != length(D$t)){
            Observations$length[i] <- length(D[[nm]])
        }
        # Its class
        Observations$class[i] <- class(D[[nm]])
        # Not ok?
        if(sum(Observations[i, 3] == "") < 1){
            Observations$ok[i] <- ""
        }
    }
    #
    # For forecasts
    dfseq <- which(dfOrMat)
    dfchecks <- c("ok","maxNAs","meanNAs","nrow","colnames","sameclass","class")
    Forecasts <- data.frame(matrix("", nrow=length(dfseq), ncol=length(dfchecks), dimnames=list(names(dfseq),dfchecks)), stringsAsFactors=FALSE)
    Forecasts$ok <- "V"
    #
    for(i in 1:length(dfseq)){
        #
        nm <- names(dfseq)[i]
        colnms <- nams(D[[nm]])
        # max NAs
        maxNAs <- round(max(sapply(colnms, function(colnm){ 100*sum(is.na(D[[nm]][ ,colnm])) / nrow(D[[nm]]) })))
        Forecasts$maxNAs[i] <- pst(maxNAs,"%")
        # Mean NAs
        meanNAs <- round(mean(sapply(colnms, function(colnm){ 100*sum(is.na(D[[nm]][ ,colnm])) / nrow(D[[nm]]) })))
        Forecasts$meanNAs[i] <- pst(meanNAs,"%")
        # Check the number of rows
        if(nrow(D[[nm]]) != length(D$t)){
            Forecasts$nrow[i] <- nrow(D[[nm]])
        }
        # Check the colnames, are they unique and all k+integer?
        if(!length(unique(grep("^k[[:digit:]]+$",colnms,value=TRUE))) == length(colnms)){
            Forecasts$colnames[i] <- "X"
        }
        if(!length(unique(sapply(colnms, function(colnm){ class(D[[nm]][ ,colnm]) }))) == 1){
            Forecasts$sameclass[i] <- "X"
        }else{
            Forecasts$class[i] <- class(D[[nm]][ ,1])
        }
        # Not ok?
        if(sum(Forecasts[i, ] == "") < (length(dfchecks)-4)){
            Forecasts$ok[i] <- ""
        }
    }
    #
    message("Observation vectors:")
    print(Observations)
    message("\nForecast data.frames or matrices:")
    print(Forecasts)

    invisible(list(Observations=Observations, Forecasts=Forecasts))
}



#' Compare two data.lists
#'
#' Returns TRUE if the two data.lists are fully identical, so all data, order of variables etc. must be fully identical
#' 
#' @title Determine if two data.lists are identical
#'
#' @param x first data.list  
#' @param y second data.list
#' @return logical
#'
#' @examples
#'
#' Dbuilding == Dbuilding
#'
#' D <- Dbuilding
#' D$Ta$k2[1] <- NA
#' Dbuilding == D
#'
#' D <- Dbuilding
#' names(D)[5] <- "I"
#' names(D)[6] <- "Ta"
#' Dbuilding == D
#' 
#' 
#' @export

"==.data.list" <- function(x, y) {
    if(length(x) != length(y)){
        return(FALSE)
    }
    if(any(names(x) != names(y))){
        return(FALSE)
    }
    # Check each variable
    tmp <- lapply(1:length(x), function(i){
        xi <- x[[i]]
        yi <- y[[i]]
        if(length(class(xi)) != length(class(yi))){
            return(FALSE)
        }
        if(any(class(xi) != class(yi))){
            return(FALSE)
        }
        if(is.null(dim(xi))){
            # It's a vector
            if(length(xi) != length(yi)){
                return(FALSE)
            }
        }else{
            # It's a data.frame or matrix
            if(any(dim(xi) != dim(yi))){
                return(FALSE)
            }
        }
        # Check the NA values are the same
        if(any(is.na(xi) != is.na(yi))){
            return(FALSE)
        }
        # Check the values
        all(xi == yi, na.rm=TRUE)
    })
    if(any(!unlist(tmp))){
        return(FALSE)
    }
    # All checks passed
    return(TRUE)
}
