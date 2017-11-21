library(R6)

#' @export
ContextualBandit <- R6Class(

  "ContextualBandit",

  inherit = MultiArmedBandit,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    k = NULL,
    action_values = NULL,
    optimal = NULL,

    initialize = function(k = NA, d = NA) {
      super$initialize(k)
      self$d = d
      self$states = matrix(0,self$k, self$d)
    },

    reset = function() {
      self$action_values = rep(k)
      self$optimal = 0
      self$states = matrix(0,self$k, self$d)
    }

  )
)
