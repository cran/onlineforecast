# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?getse


#' A helping function for getting subelemlts from a list.
#'
#' Often it is needed to get a subelement from a list, which can be done using lapply.
#' But to make life easiere here is a small function for getting subelements in a nested list at a certain debth.
#' 
#' @title Getting subelement from list.
#' @param L The list to get sub elements from.
#' @param inm Either an integer index or a name of the subelements to return.
#' @param depth The depth of the subelements to match names in:
#'     - 1: is directly in the list.
#'     - 2: is in list of each element in the list.
#'     - 3 and more: simply deeper in the sublists.
#' @param useregex logical: should inm be used as regex pattern for returning elements matching, in the specified layer.
#' @param fun function: if given, then it will be applied to all the matched subelements before returning them.
#' @return A list of the matched elements.
#' 
#' @examples
#' # Make a nested list
#' L <- list(x1=list(x=list("val11","val112"),
#'                   y=list("val12"),
#'                   test=list("testlist2")),
#'           x2=list(x=list("val21","val212"),
#'                   y=list("val22"),
#'                   test=list("testlist2")),
#'           x3=list(x=list("val31","val312"),
#'                   y=list("val32"),
#'                   test=list("testlist3")))
#'
#' # Get the subelement "x1"
#' str(getse(L, "x1", depth=1))
#' # Same as
#' str(L[["x1"]])
#'
#' # Get the element named x in second layer
#' str(getse(L, "x", depth=2))
#' # Depth is default to 2
#' str(getse(L, "y"))
#'
#' # Nice when splitting string
#' x <- strsplit(c("x.k1","y.k2"), "\\.")
#' # Get all before the split "\\."
#' getse(x, 1)
#' # Get after
#' getse(x, 2)
#'
#' # Get an element with an integer index
#' x <- strsplit(c("x.k1","y.k2","x2"), "\\.")
#' getse(x, 1)
#' # if the element is not there, then an error is thrown
#' try(getse(x, 2))
#' 
#' # Use regex pattern for returning elements matching in the specified layer
#' getse(L, "^te", depth=2, useregex=TRUE)
#' 
#' @export

getse <- function(L, inm = NA, depth = 2, useregex = FALSE, fun = NA) {
    if(depth < 0){ stop("depth has to be 1,2,3,...") }
    # Get a list of all sub elements in L matching pattern at the given depth
    # Depth==1 is directly the subelements of L, i.e. L[nms]

    # Match directly in L?
    if(depth == 1){
        if(useregex){ inm <- grep(inm, names(L)) }
        R <- L[[inm]]
        if(class(fun) == "function"){ R <- fun(R) }
    }
    # Match in the subelements of L?
    if(depth == 2){
        R <- lapply(L, function(x){
            if(useregex){ inm <- grep(inm, names(x)) }
            val <- x[[inm]]
            if(class(fun) == "function"){ val <- fun(val) }
            return(val)
        })
    }

    # Go one level deeper
    if(depth >= 3){
        R <- lapply(L, function(x){
            getse(x, inm, depth-1, useregex, fun)
        })
    }
    return(R)
}
