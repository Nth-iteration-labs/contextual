library(R6)

#' @export
Bandit <- R6Class(

  "Bandit",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    initialize = function(k = NA) {
      warning('Not implemented error.')
    },

    reset = function() {
      warning('Not implemented error.')
    },

    pull = function(action) {
      warning('Not implemented error.')
    }

  )
)
