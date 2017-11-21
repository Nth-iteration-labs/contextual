library(R6)

#' @export
Policy <- R6Class(

  "Policy",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(


    initialize = function() {
    },

    choose = function(agent){
      warning('Not implemented error.')
    }

  )
)
