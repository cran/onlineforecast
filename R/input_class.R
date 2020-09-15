input_class <- R6::R6Class(
  "input",
  list(
      ## Fields:
      ## The expression as string for transforming the input
      expr = NA,
      ## The list  and index counter holding potential state values kept by the function evaluated in the expression
      state_L = list(),
      state_i = integer(1),
      ## The model in which it is included (reference to the R6 forecastmodel object), its needed here,
      ##   since transformation functions (like AR, one) need to access information about the model (like kseq)
      model = NA,

      ## methods
      initialize = function(expr, model){
          self$expr <- expr
          self$model <- model
      },

      ## Generate (transform) the input by evaluating the expr
      evaluate = function(data){
          ## Init the state counter
          self$state_i <- 0
          ## Evaluate the expression in an environment with data
          eval(parse(text = self$expr), data)
      },

      ## For resetting the state
      state_reset = function(){
          ## Init the state counter
          self$state_i <- 0
          ## Init the state list
          self$state_L <- list()
          ##
          invisible(NULL)
      },

      ## Get the saved value in state
      state_getval = function(initval){
          self$state_i <- self$state_i + 1
          if(length(self$state_L) < self$state_i){
              ## First time called, initiate state variables
              return(initval)
          }else{
              ## Take the state saved last time
              return(self$state_L[[self$state_i]])
          }
      },

      ## Save the state for next time
      state_setval = function(val){
          self$state_L[[self$state_i]] <- val
      }
  )
)
