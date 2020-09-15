# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?cache_name

#' Caching of the value returned by a function
#'
#' Use it in the beginning of a function, which runs a time consuming calculation, like fitting a model using optimization.
#'
#' It makes a cache name, which can be used to save a unique cache file (see \code{\link{cache_save}()}).
#'
#' The \code{cache_name} function must receive all the objects (in \code{...}) which influence the value of the function. It simply calculates a checksum using the \code{digest} package.
#' 
#' Further, it finds the name of the calling function and its definition, such that if anything changes in the function definition, then the cache file name changes too.
#'
#' @title Generation of a name for a cache file for the value of a function.
#' @param ... The objects from which to calculate cache file name.
#' If no objects given, then all the objects of the calling function are used for generating the checksum for the file name.
#' @param cachedir Path for saving the cache, i.e. prefixed to the generated name, remember to end with '/' to make a directory.
#' @return A generated cache file name.
#' @examples 
#' # A function for demonstrating the using caching
#' fun <- function(x, y){
#'     # Generate the cache name (no argument given, so both x and y is used)
#'     nm <- cache_name(cachedir=cachedir)
#'     # If the result is cached, then just return it
#'     if(file.exists(nm)){ return(readRDS(nm)) }
#'     # Do the calculation
#'     res <- x^2 + y + 1
#'     # Wait 1 sec
#'     Sys.sleep(1)
#'     # Save for cache
#'     cache_save(res, nm)
#'     # Return
#'     return(res)
#' }
#'
#' # For this example use a temporary directory 
#' # In real use this should not be temporary! (changes between R sessions with tempdir())
#' cachedir <- tempdir()
#' 
#' # First time it takes at least 1 sec.
#' fun(x=2,y=2)
#' # Second time it loads the cache and is much faster
#' fun(x=2,y=2)
#' # Try changing the arguments (x,y) and run again
#'
#' # See the cache file(s)
#' dir(cachedir)
#' # Delete the cache folder
#' unlink(cachedir, recursive=TRUE)
#'
#' # Demonstrate how cache_name() is functioning
#' # Cache using the all objects given in the function calling, i.e. both x and y
#' fun <- function(x,y){
#'     x^2 + y + 1
#'     return(cache_name())
#' }
#' # These are the same (same values)
#' fun(x=1,y=2)
#' fun(1,2)
#' fun(y=2,x=1)
#' # But this one is different
#' fun(x=2,y=1)
#'
#' # Test: cache using the values specified in the cache_name call
#' fun2 <- function(x,y){
#'     x^2 + y + 1
#'     return(cache_name(x))
#' }
#'
#' # So now its only the x value that change the name
#' fun2(1,2)
#' fun2(1,3)
#' # But this one is different 
#' fun2(3,3)
#' # And the function named changed the name
#'
#' @export
cache_name <- function(..., cachedir = "cache"){
    # Get the name, definition and arguments of the function from which cache_name was called
    funname <- strsplit(deparse(sys.calls()[[sys.nframe()-1]]), "\\(")[[1]][1]
    fundef <- digest::digest(attr(eval(parse(text=funname)), "srcref"))
    # if no arguments were given, then use the arguments function from which cache_name was called
    if(length(list(...)) == 0){
        funargs <- digest::digest(as.list( match.call(definition = sys.function( -1 ), call = sys.call(-1)))[-1])
    }else{
        funargs <- digest::digest(list(...))
    }
    # Create the md5 checksum filename with digest
    filename <- paste0(funname,"_",fundef,"_",funargs,".RDS")
    return(gsub("//","/",paste0(cachedir,"/",filename)))
}
