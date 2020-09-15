#' @export
forecastmodel <- R6::R6Class("forecastmodel", public = list(
    #----------------------------------------------------------------
    # Fields used for setting up the model
    # 
    # The expression (as character) used for generating the regprm
    regprmexpr = NA,
    # Regression parameters for the function used for fitting (rls, ls, etc.)
    regprm = list(), 
    # The off-line parameters
    prmbounds = as.matrix(data.frame(lower=NA, init=NA, upper=NA)),
    # List of inputs (which are R6 objects) (note the "cloning of list of reference objects" issue below in deep_clone function)
    inputs = list(),
    # Name of the output
    output = "y",
    # The range of the output to be used for cropping the output
    outputrange = NA,
    #----------------------------------------------------------------

    
    #----------------------------------------------------------------
    # Fields to be used when the model is fitted
    #
    # The horizons to fit for
    kseq = NA,
    # The (transformation stage) parameters used for the fit
    prm = NA,
    # Stores the maximum lag for AR terms
    maxlagAR = NA,
    # Stores the maxlagAR past values of y for the update when new obs becomes available
    yAR = NA,
    # The fits, one for each k in kseq (simply a list with the latest fit)
    Lfits = list(),
    # Transformed input data (data.list with all inputs for regression)
    datatr = NA,
    #----------------------------------------------------------------

    
    #----------------------------------------------------------------
    # Contructor function
    initialize = function(){},
    #----------------------------------------------------------------

    
    #----------------------------------------------------------------    
    # Add inputs to the model
    add_inputs = function(...){
        dots <- list(...)
        for (i in 1:length(dots)){
            self$inputs[[ nams(dots)[i] ]] <- input_class$new(dots[[i]], model=self)
        }
    },
    #----------------------------------------------------------------

    #----------------------------------------------------------------
    # Add the expression (as character) which generates the regression parameters
    add_regprm = function(regprmexpr){
        self$regprmexpr <- regprmexpr
        self$regprm <- eval(parse(text = self$regprmexpr))
    },
    #----------------------------------------------------------------

    
    #----------------------------------------------------------------
    # Add the transformation parameters and bounds for optimization
    add_prmbounds = function(...) {
        dots <- list(...)
        for (i in 1:length(dots)) {
            nm <- names(dots)[i]
            if (nm %in% rownames(self$prmbounds)) {
                self$prmbounds[nm, ] <- dots[[i]]
            } else {
                if(nrow(self$prmbounds) == 1 & is.na(self$prmbounds[1,2])){
                    self$prmbounds[1, ] <- dots[[i]]
                }else{
                    self$prmbounds <- rbind(self$prmbounds, dots[[i]])
                }
                rownames(self$prmbounds)[nrow(self$prmbounds)] <- nm
            }
        }
    },
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Get the transformation parameters
    get_prmbounds = function(nm){
        if(nm == "init"){
            if(is.null(dim(self$prmbounds))){
                val <- self$prmbounds[nm]
            }else{
                val <- self$prmbounds[ ,nm]
                if(is.null(nams(val))){
                    nams(val) <- rownames(self$prmbounds)
                }
            }
        }
        if(nm == "lower"){
            if("lower" %in% nams(self$prmbounds)){
                val <- self$prmbounds[,"lower"]
                if(is.null(nams(val))){
                    nams(val) <- rownames(self$prmbounds)
                }
            }else{
                val <- -Inf
            }
        }
        if(nm == "upper"){
            if("upper" %in% nams(self$prmbounds)){
                val <- self$prmbounds[,"upper"]
                if(is.null(nams(val))){
                    nams(val) <- rownames(self$prmbounds)
                }
            }else{
                val <- Inf
            }
        }
        names(val) <- row.names(self$prmbounds)
        return(val)
    },
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Insert the transformation parameters prm in the input expressions and regression expressions, and keep them (simply string manipulation)
    insert_prm = function(prm){
        # If just NA or NULL given, then don't do anything
        if(is.null(prm) | (is.na(prm)[1] & length(prm) == 1)){
            return(NULL)
        }
        # MUST INCLUDE SOME checks here and print useful messages if something is not right
        if(any(is.na(prm))){ stop(pst("None of the parameters (in prm) must be NA: prm=",prm)) }

        # Keep the prm
        self$prm <- prm
        # Find if any opt parameters, first the one with "__" hence for the inputs
        pinputs <- prm[grep("__",nams(prm))]
        # If none found for inputs, then the rest must be for regression
        if (length(pinputs) == 0 & length(prm) > 0) {
            preg <- prm
        } else {
            preg <- prm[-grep("__",nams(prm))]
        }
        # ################
        # For the inputs, insert from prm if any found
        if (length(pinputs)) {
            pnms <- unlist(getse(strsplit(nams(pinputs),"__"), 1))
            pprm <- unlist(getse(strsplit(nams(pinputs),"__"), 2))
            #
            for(i in 1:length(self$inputs)){
                for(ii in 1:length(pnms)){
                    # Find if the input i have prefix match with the opt. parameter ii
                    if(pnms[ii]==nams(self$inputs)[i]){
                        # if the opt. parameter is in the expr, then replace
                        self$inputs[[i]]$expr <- private$replace_value(name = pprm[ii],
                                                                       value = pinputs[ii],
                                                                       expr = self$inputs[[i]]$expr)
                    }
                }
            }
        }
        # ################
        # For the fit parameters, insert from prm if any found
        if (length(preg) & any(!is.na(self$regprmexpr))) {
            nams(preg)
            for(i in 1:length(preg)){
                # if the opt. parameter is in the expr, then replace
                self$regprmexpr <- private$replace_value(name = nams(preg)[i],
                                                         value = preg[i],
                                                         expr = self$regprmexpr)
            }
        }
        self$regprm <- eval(parse(text = self$regprmexpr))
    },
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Function for transforming the input data to the regression data
    transform_data = function(data){
        # Evaluate for each input the expresssion to generate the model input data
        L <- lapply(self$inputs, function(input){
            # Evaluate the expression (input$expr)
            L <- input$evaluate(data)
            # Must return a list
            if(class(L)[1]=="matrix"){ return(list(as.data.frame(L))) }
            if(class(L)[1]=="data.frame"){ return(list(L)) }
            if(class(L)[1]!="list"){ stop(pst("The value returned from evaluating: ",input$expr,", was not a matrix, data.frame or a list of them."))}
            if(class(L[[1]])[1]=="matrix"){ return(lapply(L, function(mat){ return(as.data.frame(mat)) })) }
            return(L)
        })
        # Put together in one data.list
        L <- structure(do.call(c, L), class="data.list")
        #
        return(L)
    },
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Resets the input states
    reset_state = function(){
        # Reset the inputs state
        lapply(self$inputs, function(input){
            input$state_reset()
        })
        # Reset stored data
        self$datatr <- NA
        self$yAR <- NA
    },
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Check if the model and data is setup correctly
    check = function(data = NA){
        # some checks are done here, maybe more should be added (??also when transforming inputs, if something goes wrong its caught and message is printed)
        #
        # ################################
        # First check if the output is set correctly
        if( is.na(self$output) ){
            stop("Model output is NA, it must be set to the name of a variable in the data.list used.")
        }
        if( !(self$output %in% names(data)) ){
            stop("Model output '",self$output,"' is not in the data provided: It must be set to the name of a variable in the data.list used.")
        }
        if( !(is.numeric(data[[self$output]])) ){
            stop("The model output '",self$output,"' is not a numeric. It has to be a vector of numbers.")
        }
        if( length(data[[self$output]]) != length(data$t) ){
            stop("The length of the model output '",self$output,"' is ",length(data[[self$output]]),", which is not equal to the length of the time vector (t), which is ",length(data$t))
        }
        # ################################
        # Check that the kseq is set in the model
        if( !is.numeric(self$kseq) ){
            stop("'model$kseq' is not set. Must be an integer (or numeric) vector.")
        }
        # ################################
        # Check all input variables are correctly set data
        for(i in 1:length(self$inputs)){
            # Find all the variables in the expression
            nms <- all.vars(parse(text=self$inputs[[i]]$expr[[1]]))
            for(nm in nms){
                if(class(data[[nm]]) %in% c("data.frame","matrix")){
                    # It's a forecast input, hence must have the k columns in kseq
                    if(!all(self$kseq %in% as.integer(gsub("k","",names(data[[nm]]))))){
                        missingk <- which(!self$kseq %in% as.integer(gsub("k","",names(data[[nm]]))))
                        stop("The input variable '",nm,"' doesn't have all needed horizons.\nIt has ",pst(names(data[[nm]]),collapse=","),"\nIt is missing ",pst("k",self$kseq[missingk],collapse=","))
                    }
                    # Check if the number of observations match
                    if( nrow(data[[nm]]) != length(data$t) ){
                        stop(pst("The input variable '",nm,"' doesn't have the same number of observations as time vector 't' in the data. It has ",nrow(data[[nm]]),", but 't' has ",length(data$t)))
                    }
                }else if(class(data[[nm]]) == "numeric"){
                    # Observation input, check the length
                    if( length(data[[nm]]) != length(data$t) ){
                        stop("The input variable '",nm,"' doesn't have the same number of observations as time vector 't' in the data. It has ",length(data[[nm]]),", but 't' has ",length(data$t))
                    }
                }else{
                    stop("The variable '",nm,"' is missing in data, or it has the wrong class.\nIt must be class: data.frame, matrix or vector.\nIt is needed for the input expression '",self$inputs[[i]]$expr[[1]],"'")
                }
            }
        }
    },

    #----------------------------------------------------------------
    clone_deep = function(){
        # First clone with deep=TRUE. Now also the inputes get cloned.
        newmodel <- self$clone(deep=TRUE)
        # The inputs are cloned now, however the model fields in the inputs have not been updated, so do that
        if(length(newmodel$inputs) > 0){
            for(i in 1:length(newmodel$inputs)){
                newmodel$inputs[[i]]$model <- newmodel
            }
        }
        return(newmodel)
    }
    #----------------------------------------------------------------
    
    ),
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Private functions
    private = list(
    #----------------------------------------------------------------


    #----------------------------------------------------------------
    # Replace the value in "name=value" in expr
    replace_value = function(name, value, expr){
        # First make regex
        pattern <- gsub("\\.", ".*", name)
        # Try to find it in the input
        pos <- regexpr(pattern, expr)
        # Only replace if prm was found
        if(pos>0){
            pos <- c(pos+attr(pos,"match.length"))
            # Find the substr to replace with the prm value
            (tmp <- substr(expr, pos, nchar(expr)))
            pos2 <- regexpr(",|)", tmp)
            # Insert the prm value and return
            expr <- pst(substr(expr,1,pos-1), "=", value, substr(expr,pos+pos2-1,nchar(expr)))
            # Print? Not used now
            #if(printout){ message(names(value),"=",value,", ",sep="")}
        }
        return(expr)
    },
    #----------------------------------------------------------------

    #----------------------------------------------------------------
    # For deep cloning, in order to get the inputs list of R6 objects copied
    deep_clone = function(name, value) {
        # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
        # each field, with the name and value.
        if (name == "inputs") {
            # Don't clone the inputs deep, since they have the model as a field and then it gets in an infinitie loop!
            # But have to update the model references, so therefore the function above "clone_deep" must be used
            return(lapply(value, function(x){ x$clone(deep=FALSE) }))
            # # `a` is an environment, so use this quick way of copying
            # list2env(as.list.environment(value, all.names = TRUE),
            #          parent = emptyenv())
        }
        # For all other fields, just return the value
        return(value)
    }
    #----------------------------------------------------------------
    )
)



#' Prints a forecast model
#'
#' A simple print out of the model output and inputs
#' 
#' @title Print forecast model
#' @param x A forecastmodel object
#' @param ... Not used.
#' 
#' @export
print.forecastmodel <- function(x, ...){
    model <- x
    #    cat("\nObject of class forecastmodel (R6::class)\n\n")
    cat("\nOutput:",model$output)
    cat("Inputs: ")
    if(length(model$inputs) == 0 ){
        cat("No inputs\n")
    }else{
        cat(names(model$inputs)[1],"=",model$inputs[[1]]$expr,"\n")
        for(i in 2:length(model$inputs)){
            cat("       ",names(model$inputs)[i],"=",model$inputs[[i]]$expr,"\n")
        }
        cat("\n")
    }
}
