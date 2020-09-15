## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?AR

#' Generate auto-regressive (AR) inputs in a model
#'
#' The AR function can be used in an onlineforecast model formulation. It
#' creates the input matrices for including AR inputs in a model during the
#' transformation stage. It takes the values from the model output in the provided data
#' does the needed lagging.
#'
#' The lags must be given according to the one-step ahead model, e.g.:
#'
#' \code{AR(lags=c(0,1))} will give: Y_{t+1|t} = \eqn{\phi_1} y_{t-0} + \eqn{\phi_2} y_{t-1} + \eqn{\epsilon}_{t+1}
#'
#' and:
#'
#' \code{AR(lags=c(0,3,12))} will give: Y_{t+1|t} = \eqn{\phi}_1 y_{t-0} + \eqn{\phi}_2 y_{t-3} + \eqn{\phi}_3 y_{t-12} + \eqn{\epsilon}_{t+1}
#'
#' Note, that 
#'
#' For k>1 the coefficients will be fitted individually for each horizon, e.g.:
#' 
#' \code{AR(lags=c(0,1))} will be the multi-step AR: Y_{t+k|t} = \eqn{\phi}_{1,k} y_{t-0} + \eqn{\phi}_{2,k} y_{t-1} + \eqn{\epsilon}_{t+k|t}
#'
#' See the details in ??(ref til vignette).
#' 
#' @title Auto-Regressive (AR) input
#' @param lags integer vector: The lags of the AR to include.
#' @return A list of matrices, one for each lag in lags, each with columns according to model$kseq.
#' @examples
#'
#' # Setup data and a model for the example
#' D <- Dbuilding
#' model <- forecastmodel$new()
#' model$output = "heatload"
#' # Use the AR in the transformation stage
#' model$add_inputs(AR = "AR(c(0,1))")
#' # Regression parameters
#' model$add_regprm("rls_prm(lambda=0.9)")
#' # kseq must be added
#' model$kseq <- 1:4
#' # In the transformation stage the AR input will be generated
#' # See that it generates two input matrices, simply with the lagged heat load at t for every k
#' model$transform_data(subset(D, 1:10))
#'
#' # Fit with recursive least squares (no parameters prm in the model)
#' fit <- rls_fit(c(lambda=0.99), model, D, returnanalysis=TRUE)
#'
#' # Plot the result, see "?plot_ts.rls_fit"
#' plot_ts(fit, xlim=c(ct("2010-12-20"),max(D$t)))
#' # Plot for a short period with peaks
#' plot_ts(fit, xlim=c("2011-01-05","2011-01-07"))
#'
#' # For online updating, see ??ref{vignette, not yet available}:
#' # the needed lagged output values are stored in the model for next time new data is available
#' model$yAR
#' # The maximum lag needed is also kept
#' model$maxlagAR
#'
#' @export

AR <- function(lags){
    # Just sort them first
    lags <- sort(lags)
    # Get the data and the model from an environment above (this way has worked until now, not exactly sure why the environments are in this order)
    data <- parent.env(parent.frame())$data
    model <- parent.env(parent.frame())$self$model
    
    # Remember the max lag for later, only if bigger than current (should make set function doing this check)
    if(is.na(model$maxlagAR) | max(lags) > model$maxlagAR){
        model$maxlagAR <- max(lags)
    }

    # Setup the AR inputs, one matrix for each lag
    retval <- lapply(lags, function(lag){
        # Check if saved output values for AR exists
    	if(is.na(model$yAR[1])){
            # First time its called, so just use output values from data
            val <- matrix(lagvec(data[[model$output]], lag), nrow=length(data$t), ncol=length(model$kseq))
    	}else{
            y <- c(model$yAR, data$y)
            # Find the seq for the new y lagged vector
            iend <- (length(y)-lag)
            istart <- iend - length(data$y) + 1
            # Take the sequence
            y <- y[istart:iend]
            # Insert in a matrix with column for each k
            val <- matrix(y, nrow=length(data$t), ncol=length(model$kseq))
        }
        # Name the columns and return
    	nams(val) <- pst("k", model$kseq)
    	return(val)
    })
    names(retval) <- pst("lag", lags)
    return(retval)
}
