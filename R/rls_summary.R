#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?rls_summary

#' The summary of an onlineforecast model fitted with RLS with simple stats providing a simple overview.
#'
#' The following is printed:
#' 
#' * The model.
#'
#' * Number of observations included in the scoreperiod.
#'
#' * RLS coefficients summary statistics for the estimated coefficient time series (since observations are correlated, then usual statistics cannot be applied directly):
#'
#'     - mean: the sample mean of the series.
#'
#'     - sd: sample standard deviation of the series.
#'
#'     - min: minimum of the series.
#'
#'     - max: maximum of the series.
#'
#' * Scorefunction applied for each horizon, per default the RMSE.
#'
#' @title Print summary of an onlineforecast model fitted with RLS
#' @param object of class \code{rls_fit}, so a fit calculated by \code{\link{rls_fit}}.
#' @param scoreperiod logical (or index). If this scoreperiod is given, then it will be used over the one in the fit.
#' @param scorefun The score function to be applied on each horizon.
#' @param usecomplete Use on the set of observations which is complete on all horizons.
#' @param printit Print the result.
#' @param ... Not used.
#' @return A list of:
#' 
#'     - scorefun.
#' 
#'     - scoreval (value of the scorefun for each horizon).
#' 
#'     - scoreperiod is the scoreperiod used.
#'
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # Define a model
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "Ta",
#'                  mu = "one()")
#' model$add_regprm("rls_prm(lambda=0.99)")
#' model$kseq <- 1:6
#' # Fit it
#' fit <- rls_fit(prm=c(lambda=0.99), model, D)
#'
#' # Print the summary
#' summary(fit)
#' # We see:
#' #   - The model (output, inputs, lambda)
#' #   - The Ta coefficient is around -0.12 in average (for all horizons) with a standard dev. of 0.03,
#' #     so not varying extremely (between -0.18 and -0.027).
#' #   - The intercept mu is around 5.5 and varying very little.
#' #   - The RMSE is around 0.9 for all horizons.
#'
#' # The residuals and coefficient series can be seen by
#' plot_ts(fit)
#'
#' @importFrom stats sd
#' @export
rls_summary <- function(object, scoreperiod = NA, scorefun = rmse, usecomplete = TRUE, printit = TRUE, ...){
    fit <- object
    #
    scipen <- options(scipen=10)$scipen
    # 
    tmp <- score_fit(fit, scoreperiod, usecomplete, scorefun)
    scoreval <- tmp$scoreval
    scoreperiodused <- tmp$scoreperiod
    retval <- list(scorefun = scorefun, scoreval = scoreval, scoreperiod = scoreperiodused)
    # Return the result before print?
    if(!printit){
        return(retval)
    }
    # Insert the optimized parameters
    m <- fit$model$clone_deep()
    m$prm[names(m$prm)] <- signif(m$prm, digits=3)
    m$insert_prm(m$prm)
    print(m)
    #
    cat("Regression parameters:\n")
    for(i in 1:length(m$regprm)){
        cat("    ",names(m$regprm)[i],"=",unlist(m$regprm[i]),"\n")
    }
    #
    cat("\nScoreperiod:",sum(scoreperiodused),"observations are included.\n")
    #
    cat("\nRLS coeffients summary stats (cannot be used for significance tests):\n")
    coef <- t(sapply(1:length(fit$Lfitval[[1]]), function(i){
        val <- sapply(fit$Lfitval, function(Theta){
            Theta[scoreperiodused,i]
        })
        #
        m <- mean(val,na.rm=TRUE)
        s <- sd(val,na.rm=TRUE)
        #abscv <- abs(s/m)
        # # An AR1 coefficient can tell a bit about the behaviour of the coefficient
        # x <- c(val)
        # xl1 <- lagdf(x,1)
        #
        c(mean=m, sd=s, min=min(val,na.rm=TRUE), max=max(val,na.rm=TRUE)) #coefvar=abscv, skewness=skewness(val, na.rm=TRUE))#, ar1=unname(lm(x ~ xl1)$coefficients[2]))
    }))
    rownames(coef) <- names(fit$Lfitval[[1]])
    print(signif(coef, digits=2))
    options(scipen=scipen)
    #
    # Print the score
    if("scorefun" %in% names(as.list(match.call()))){
        scorename <- as.list(match.call())$scorefun
    }else{
        scorename = "rmse"
    }
    if( any(scoreval < 10) ){
        tmp <- signif(scoreval, digits=2)
    }else{
        tmp <- round(scoreval, digits=1)
    }
    cat(pst("\n",toupper(scorename),":\n"))
    print(tmp)
    cat("\n")
    invisible(list(scorefun = scorefun, scoreval = scoreval, scoreperiod = scoreperiodused))
}

#' @importFrom stats sd
#' @export
summary.rls_fit <- rls_summary
