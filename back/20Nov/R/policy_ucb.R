library(R6)

#' @export
UCBPolicy <- R6Class(

  "UCBPolicy",

  inherit = Policy,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    c = NULL,

    initialize = function(c=2) {

      self$c = c

    },

    choose = function(agent){

      exploration = 2 * log(agent$t + 1) / agent$action_attempts

      exploration[is.nan(exploration)] = 0

      exploration = exploration ^ (1 / self$c)

      ucb = agent$theta + exploration

      action = index_of_max(ucb)

      return(action)
    }

  )
)
