# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?score

#' Calculates the score for each horizon for a matrix with residuals for each horizon.
#'
#' Applies the \code{scorefun} on all horizons (each column) of the residuals matrix. See the description of each parameter for more details.
#' 
#' @title Calculate the score for each horizon.
#' @param Residuals A matrix with residuals (columns named \code{hxx}) for which to calculate the score for each horizon.
#' @param scoreperiod as a logical vector controlling which points to be included in the score calculation. If NA then all values are included.
#' @param usecomplete if TRUE then only the values available for all horizons are included (i.e. if at one time point there is a missing value, then values for this time point is removed for all horizons in the calculation).
#' @param scorefun The score function.
#' @return A list with the a numeric vector with the score value for each horizon and the applied \code{scoreperiod} (note can be different from the given scoreperiod, if only complete observations are used (as per default)).
#' @examples
#'
#' # Just a vector to be forecasted
#' y <- c(filter(rnorm(100), 0.95, "recursive"))
#' # Generate a forecast matrix with a simple persistence model
#' Yhat <- persistence(y, kseq=1:4)
#' # The residuals for each horizon
#' Resid <- residuals(Yhat, y)
#'
#' # Calculate the score for the k1 horizon
#' score(Resid)$scoreval
#'
#' # The first values were excluded, since there are NAs
#' head(Resid)
#' score(Resid)$scoreperiod
#'
#' @importFrom stats complete.cases
#' @export
score <- function(Residuals, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse){
    # If no scoreperiod is given, then use all
    if(is.na(scoreperiod[1])){
        scoreperiod <- rep(TRUE,nrow(Residuals))
    }else{
        # Do checking of scoreperiod
        txt <- "It must be set to an index (int or logical) defining which points to be evaluated in the scorefun()."
        if( length(scoreperiod) != nrow(Residuals) ){
            stop("scoreperiod is not same length as nrow(Residuals): ",txt)
        }else{
            if( all(is.na(scoreperiod)) ){ stop("scoreperiod is all NA: ",txt) }
        }
    }
    # Take only the rows which have a value for each horizon?
    if(usecomplete){
        scoreperiod <- scoreperiod & complete.cases(Residuals)
    }
    # Calculate the objective function for each horizon
    scoreval <- sapply(1:ncol(Residuals), function(i){
        scorefun(Residuals[scoreperiod,i])
    })
    nams(scoreval) <- gsub("h","k",nams(Residuals))
    # 
    return(list(scoreval=scoreval,scoreperiod=scoreperiod))
}
