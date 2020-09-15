#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?rls_update

#' Calculates the RLS update of the model coefficients with the provived data.
#'
#' See vignette ??ref(recursive updating, not yet finished) on how to use the function. 
#' 
#' @title Updates the model fits
#' @param model A model object
#' @param datatr a data.list with transformed data (from model$transform_data(D))
#' @param y A vector of the model output for the corresponding time steps in \code{datatr}
#' @param runcpp Optional, default = TRUE. If TRUE, a c++ implementation of the update is run, otherwise a slower R implementation is used.
#' @return
#'
#' Returns a named list for each horizon (\code{model$kseq}) containing the variables needed for the RLS fit (for each horizon, which is saved in model$Lfits):
#'
#' It will update variables in the forecast model object.
#'
#' @seealso
#' See \code{\link{rls_predict}}.
#' 
#' @examples
#'
#' # See rls_predict examples
#'
#' @export

rls_update <- function(model, datatr = NA, y = NA, runcpp=TRUE) {
    # Take the inputs data and bind with the kept inputs data in the fit
    #
    # The data must be kept for later updating, done below
    # The last part of the input data is needed for next update

    # Find the number of parameters for the regression
    np <- length(datatr)

    # Keep only the last kmax rows for next time
    kmax <- max(as.integer(gsub("k", "", nams(datatr[[1]]))))

    # Check if data was kept
    kept_input_data <- !is.na(model$datatr[1])
    #
    if (kept_input_data) {
        # Find the start index for iterating later (the index to start updating from)
        # How many points are kept plus one
        istart <- nrow(model$datatr[[1]]) + 1
        # Bind together new and kept data
        for (i in 1:length(datatr)) {
            # Bind them
            datatr[[i]] <- rbind(model$datatr[[i]], datatr[[i]])
            # Keep only the last kmax rows for next time
            # Done below: model$datatr[[i]] <- datatr[[i]][(n+1):(kmax+n), ]
        }
        # Also for y to sync with X
        y <- c(rep(NA,istart-1), y)
    } else {
        # Set later when nothing is kept (it must be set to k+1)
        istart <- NA
    }

    # The number of points
    n <- length(y)

    # Parameters for rls
    lambda <- model$regprm$lambda

    if(runcpp){
        L <- lapply(model$Lfits, function(fit) {

            # Take the needed values from the fit
            k <- fit$k
            theta <- fit$theta
            # The non cpp keeps R, see below
            if(is.null(fit$P)){
                P <- solve(fit$R)
            }else{ P <- fit$P }

            # Form the regressor matrix, don't lag it
            X <- as.matrix(as.data.frame(subset(datatr, kseq=k)))

            # When nothing was kept
            if(!kept_input_data){ istart <- k + 1 }
            val <- rls_update_cpp(y, X, theta, P, lambda, k, n, np, istart, kmax)
            # Give names to the matrices (maybe faster if done in cpp function, see in the end)
            colnames(val$fit$P) <- names(datatr)
            colnames(val$result$Theta) <- names(datatr)
            # Give the result
            return(val)
        })
    }else{
        # Fit the model for each k
        L <- lapply(model$Lfits, function(fit) {
            # Take the needed values from the fit
            k <- fit$k
            theta <- fit$theta
            # The cpp keeps P
            if (is.null(fit$R)) {
                R <- solve(fit$P)
            } else {
                R <- fit$R
            }

            # Form the regressor matrix, don't lag it
            X <- as.data.frame(subset(datatr, kseq=k))

            # Prepare for keeping for the parameter estimates
            Theta <- matrix(as.numeric(NA), nrow = n, ncol = np)

            # Make vector for predictions k steps ahead
            yhat <- rep(NA,length(y))
            # If input data was kept (i.e. it is not a fresh update), NAs was added above in X and y, so insert the kept yhat
            if(kept_input_data){ yhat[1:length(fit$yhat)] <- fit$yhat}

            # When nothing was kept
            if(!kept_input_data){ istart <- k + 1 }

            # Iterate through
            for (i in istart:n) {
                # Take the forecast k steps back to match it with y[i]
                x <- t(as.matrix(X[i-k, ]))

                if(!any(is.na(x)) & !is.na(y[i])){
                    # Update
                    R <- lambda * R + x %*% t(x)
                    theta <- theta + solve(R, x) %*% (y[i] - t(x) %*% theta)
                    Theta[i, ] <- t(theta)
                }

                # Make a prediction
                yhat[i] <- as.matrix(X[i, ]) %*% theta
            }
            #
            # Give names to the matrices
            colnames(R) <- names(datatr)
            colnames(Theta) <- names(datatr)
            # Return the fit and result
            return(list(fit=list(k=k, theta=theta, R=R, yhat=yhat[(n-kmax+1):n]),
                        result=list(yhat=yhat, Theta=Theta)))
        })
    }
    #
    # Keep the last part of the transformed data for later
    model$datatr <- subset(datatr, (n-kmax+1):n)
    # Store the values of y if the model has AR term
    if(!is.na(model$maxlagAR)){
        # Was data kept?
        if(kept_input_data){
            # Yes, so put together with the kept
            tmpy <- c(model$yAR, y[istart:n])
        }else{
            # No, then just take from y
            tmpy <- y
        }
        # In case too few new values, then fill with NAs
        if((model$maxlagAR+1) > length(tmpy)){ tmpy <- c(rep(NA,(model$maxlagAR+1)-length(tmpy)), tmpy) }
        # Keep the needed
        model$yAR <- tmpy[(length(tmpy)-model$maxlagAR):length(tmpy)]
    }
    #
    # Keep the fit
    model$Lfits <- getse(L, "fit")
    # Return Theta in a list for each k
    invisible(getse(L, "result"))
}
