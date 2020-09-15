#' Use a fitted forecast model to predict its output variable with transformed data.
#'
#' See the ??ref(recursive updating vignette, not yet available).
#'
#' @title Prediction with an rls model.
#' @param model Onlineforecast model object which has been fitted.
#' @param datatr Transformed data.
#' @return The Yhat forecast matrix with a forecast for each model$kseq and for each time point in \code{datatr$t}.
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "Ta", mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#'
#' # Before fitting the model, define which points to include in the evaluation of the score function
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # And the sequence of horizons to fit for
#' model$kseq <- 1:6
#'
#' # Transform using the mdoel
#' datatr <- model$transform_data(D)
#'
#' # See the transformed data
#' str(datatr)
#'
#' # The model has not been fitted
#' model$Lfits
#'
#' # To fit
#' rls_fit(model=model, data=D)
#'
#' # Now the fits for each horizon are there (the latest update)
#' # For example the current parameter estimates
#' model$Lfits$k1$theta
#'
#' # Use the current values for prediction
#' D$Yhat <- rls_predict(model, datatr)
#'
#' # Plot it
#' plot_ts(D, c("y|Yhat"), kseq=1)
#'
#' # Recursive updating and prediction
#' Dnew <- subset(Dbuilding, c("2011-01-01", "2011-01-02"))
#'
#' for(i in 1:length(Dnew$t)){
#'     # New data arrives
#'     Dt <- subset(Dnew, i)
#'     # Remember that the transformation must only be done once if some transformation
#'     # which is has a state, e.g. lp(), is used
#'     datatr <- model$transform_data(Dt)
#'     # Update, remember that this must only be once for each new point
#'     # (it updates the parameter estimates, i.e. model$Lfits)
#'     rls_update(model, datatr, Dt$heatload)
#'     # Now predict to generate the new forecast
#'     print(rls_predict(model, datatr))
#' }
#'
#' @export
rls_predict <- function(model, datatr = NA) {
    # - model: the model object
    # - datatr: is a datalist which holds the transformed inputs

    # Predict with the model for each k
    Yhat <- sapply(model$kseq, function(k) {
        # Take the fit for k
        theta <- model$Lfits[[pst("k",k)]]$theta
        # Form the regressor matrix
        X <- sapply(datatr, function(x) {
            x[, pst("k", k)]
        })
        # Catch if only one row, then X is vector, convert to matrix
        if (is.null(dim(X))) {
            X <- matrix(X, ncol = length(X), dimnames = list(NULL, nams(X)))
        }
        # The predictions
        yhat <- as.numeric(rep(NA, nrow(X)))
        #
        iOk <- which(apply(is.na(X), 1, sum) == 0)
        for (i in iOk) {
            x <- matrix(X[i, ])
            # Predict
            yhat[i] <- t(x) %*% theta
        }
        return(yhat)
    })
    if (is.null(dim(Yhat))) {
        Yhat <- matrix(Yhat, ncol = length(Yhat), dimnames = list(NULL, nams(Yhat)))
    }
    Yhat <- as.data.frame(Yhat)
    nams(Yhat) <- pst("k", model$kseq)
    # Maybe crop the output
    if(!is.na(model$outputrange[1])){ Yhat[Yhat < model$outputrange[1]] <- model$outputrange[1] }
    if(!is.na(model$outputrange[2])){ Yhat[model$outputrange[1] > Yhat] <- model$outputrange[2] }
    #
    return(Yhat)
}
