## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?

#' Helper which does lapply and then cbind
#' @param X object to apply on
#' @param FUN function to apply
#' @export
lapply_cbind <- function(X, FUN){
  val <- lapply(X, FUN)
  return(do.call("cbind", val))
}

#' Helper which does lapply and then rbind
#' @param X object to apply on
#' @param FUN function to apply
#' @export
lapply_rbind <- function(X, FUN){
  val <- lapply(X, FUN)
  return(do.call("rbind", val))
}

#' Helper which does lapply, cbind and then as.data.frame
#' @param X object to apply on
#' @param FUN function to apply
#' @export
lapply_cbind_df <- function(X, FUN){
  val <- lapply(X, FUN)
  return(as.data.frame(do.call("cbind", val)))
}

#' Helper which does lapply, rbind and then as.data.frame
#' @param X object to apply on
#' @param FUN function to apply
#' @export
lapply_rbind_df <- function(X, FUN){
  val <- lapply(X, FUN)
  return(as.data.frame(do.call("rbind", val)))
}

