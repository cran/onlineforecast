#' Saves the object as an .RDS file with the filename
#'
#' See the examples for \code{\link{cache_name}()}.
#'
#' @title Save a cache file (name generated with \code{code_name()}
#' @param object The object to cache (i.e. the value of the evaluating function).
#' @param filename The cache file name (i.e. use the one generated by cache_name, see examples).
#' @return NULL
#'
#' @export
cache_save <- function(object, filename){
    dir.create(dirname(filename), showWarnings=FALSE, recursive=TRUE)
    saveRDS(object, filename)
}



