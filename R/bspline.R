# # Do this in a separate file to see the generated help:
# library(devtools)
# document()
# load_all(as.package("../../onlineforecast"))
# ?bspline


#' Compute base splines of a variable using the R function \code{splines::bs}, use in the transform stage.
#'
#' Simply wraps the \code{splines::bs}, such that it can be used in the transformation stage.
#'
#' See the help for all arguments with \code{?splines::bs}. NOTE that two arguments have different default values.
#'
#' See the example \url{https://onlineforecasting.org/examples/solar-power-forecasting.html} where the function is used in a model.
#' 
#' @family Transform stage functions
#' 
#' @param X data.frame (as part of data.list) with horizons as columns named \code{kxx} (i.e. one for each horizon)
#' @param Boundary.knots The value is NA: then the boundaries are set to the range of each horizons (columns in X). See \code{?splines::bs}
#' @param intercept See \code{?splines::bs}.
#' @param df See \code{?splines::bs}
#' @param knots See \code{?splines::bs}
#' @param degree See \code{?splines::bs}
#' @param bknots Is just a short for Boundary.knots and replace Boundary.knots (if Boundary.knots is not given)
#' @param periodic Default FALSE. If TRUE, then \code{pbs::pbs} is called and periodic splines are generated.
#' @return List of data frames with the computed base splines, each with columns for the same horizons as in X
#' @rdname bs
#' @examples
#'
#' # How to make a diurnal curve using splines
#'  # Select first 54 hours from the load data
#'  D <- subset(Dbuilding, 1:76, kseq=1:4)
#'  # Make the hour of the day as a forecast input
#'  D$tday <- make_tday(D$t, kseq=1:4)
#'  D$tday
#' 
#'  # Calculate the base splines for each column in tday
#'  L <- bspline(D$tday)
#'
#'  # Now L holds a data.frame for each base spline
#'  str(L)
#'  # Hence this will result in four inputs for the regression model
#'
#'  # Plot (note that the splines period starts at tday=0)
#'  plot(D$t, L$bs1$k1, type="s")
#'  for(i in 2:length(L)){
#'    lines(D$t, L[[i]]$k1, col=i, type="s")
#'  }
#'
#'  # In a model formulation it will be:
#'  model <- forecastmodel$new()
#'  model$add_inputs(mutday = "bspline(tday)")
#'  # Such that at the transform stage will give the same as above
#'  model$transform_data(D)
#'
#' # Periodic splines are useful for modelling a diurnal harmonical functions
#' L <- bspline(D$tday, bknots=c(0,24), df=4, periodic=TRUE)
#' # or
#' L <- pbspline(D$tday, bknots=c(0,24), df=4)
#' # Note, how it has to have high enough df, else it generates an error
#'
#' # Plot
#' plot(D$t, L$bs1$k1, type="s")
#' for(i in 2:length(L)){
#'     lines(D$t, L[[i]]$k1, col=i, type="s")
#' }
#'
#' @export
bspline <- function(X, Boundary.knots = NA, intercept = FALSE, df = NULL, knots = NULL, degree = 3, bknots = NA, periodic = FALSE) {
    # bknots is just a short for Boundary.knots and replace if Boundary.knots are not given.
    if(is.na(Boundary.knots)){
        Boundary.knots <- bknots
    }
    # If a list, then call on each element
    if (class(X) == "list") {
        # Call again for each element
        val <- lapply(1:length(X), function(i) {
            bspline(X[[i]], df = df, knots = knots, degree = degree, intercept = intercept, 
                    Boundary.knots = Boundary.knots, periodic = periodic)
        })
        nams(val) <- nams(X)
        return(val)
    }
    # X is a data.frame or matrix
    # First find the horizons, they are used in the end
    nms <- nams(X)
    # Run for each horizon and calculate the basis splines
    L <- lapply(1:ncol(X), function(i) {
      if (is.na(Boundary.knots[1])) {
          Boundary.knots <- range(X[, i], na.rm=TRUE)
      }
      if(periodic){
          # Periodic splines (annoyingly it use 'missing()' to conclude if the argument was given, not 'is.null()' as bs does)
          if(is.null(knots)){
              # Call without knots
              spls <- pbs::pbs(X[, i], Boundary.knots = Boundary.knots, degree = degree, df = df,
                               intercept = intercept)
          }else{
              # Call without df
              spls <- pbs::pbs(X[, i], Boundary.knots = Boundary.knots, degree = degree,
                               knots = knots, intercept = intercept)
          }
      }else{
          spls <- splines::bs(X[, i], Boundary.knots = Boundary.knots, degree = degree, df = df,
                              knots = knots, intercept = intercept)
      }
      return(spls)
    })
    # Now we have a bs value in L for each horizon
    # Separate each basespline in a data.frame with all horizons
    L <- lapply(1:ncol(L[[1]]), function(i) {
      tmp <- lapply(L, function(x) {
        x[ ,i]
      })
      tmp <- data.frame(do.call("cbind", tmp))
      nams(tmp) <- nms
      return(tmp)
    })
    # Set the extra name
    nams(L) <- pst("bs", 1:length(L))
    return(L)
}

#' Wrapper for \code{bspline} with \code{periodic=TRUE}
#'
#' Simply a wrapper.
#' 
#' @title Wrapper for \code{bspline} with \code{periodic=TRUE}
#' @param X see \code{?bspline}
#' @param Boundary.knots see \code{?bspline}
#' @param intercept see \code{?bspline}
#' @param df see \code{?bspline}
#' @param knots see \code{?bspline}
#' @param degree see \code{?bspline}
#' @param bknots see \code{?bspline}
#' @family Transform stage functions
#'
#' @export
pbspline <- function(X, Boundary.knots = NA, intercept = FALSE, df = NULL, knots = NULL, degree = 3, bknots = NA){
    bspline(X, Boundary.knots = Boundary.knots, degree = degree, df = df,
            knots = knots, intercept = intercept, bknots = bknots, periodic = TRUE)
}
    
