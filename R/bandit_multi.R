library(R6)

#' @export
MultiBandits <- R6Class(

  "MultiBandits",

  inherit = Agent,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    k = NULL,
    action_values = NULL,
    optimal = NULL,

    initialize = function() {

      self$reset()
    },

    reset = function() {
      self$bandits = vector(mode = "numeric", length = 0)
      self$bandit = NA
      self$cursor = 0
      self$k = 0
    },

    add_bandit = function(bandit) {
      self$bandits.append(bandit)
      self$k = bandit$k
    }

  )
)
