#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?rls_prm

#' Function for generating the parameters for RLS regression
#'
#' The RLS needs only a forgetting factor parameter.
#' 
#' @title Function for generating the parameters for RLS regression
#' @param lambda The forgetting factor
#' @return A list of the parameters
#' @examples
#'
#' # Take data
#' D <- subset(Dbuilding, c("2010-12-15", "2011-01-01"))
#' D$y <- D$heatload
#' D$scoreperiod <- in_range("2010-12-20", D$t)
#' # Define a simple model 
#' model <- forecastmodel$new()
#' model$add_inputs(Ta = "Ta", mu = "one()")
#' model$kseq <- 1:6
#'
#' # Here the expression which sets the parameters is defined
#' model$add_regprm("rls_prm(lambda=0.99)")
#' model$regprmexpr
#'
#' # These will fit with lambda=0.99
#' rls_fit(prm=NA, model, D)
#' rls_fit(prm=c(lambda=0.99), model, D)
#'
#' # The expression is evaluated when the model is fitted
#' rls_fit(prm=c(lambda=0.85), model, D)
#'
#' # What happens is simply that the expression was manipulated
#' model$regprmexpr
#' model$regprm
#'
#' # Same change could be done by
#' model$regprm <- list(lambda=0.3)
#' model$regprm
#' val <- rls_fit(prm=NA, model, D)
#' 
#' @export
rls_prm <- function(lambda) {
    list(lambda = lambda)
}
