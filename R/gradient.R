library(R6)

#' @export
Agent <- R6Class(

  "Agent",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    bandit = NULL,
    action_attempts = NULL,
    last_action = NULL,
    prior=0, gamma=None

    initialize = function(
      policy = NA,
      bandit = NA
    ) {

      self$bandit = bandit
      self$policy = policy
      self$reset()

    },

    reset = function() {
      private$theta = rep(0, bandit$k)
      self$action_attempts = rep(0, bandit$k)
      self$last_action = NA

    },

    # check how to diff between context of 1 AND multiple
    get_action = function(context) {
      action = self$policy$get_action(self)
      self$last_action = action
      return(action)
    },

    get_theta = function() {
        return(private$theta)
    },

    set_reward = function(reward) {

      self$action_attempts[self$last_action] =
        self$action_attempts[self$last_action] + 1

      if (is.na(self$gamma)) {
        g = 1 / self$action_attempts[self$last_action]
      } else {
        g = self$gamma
      }
      q = private$theta[self$last_action]

      private$theta[self$last_action] =
        private$theta[self$last_action] + g * (reward - q)

      self$t = self$t + 1
    }

  ),
  private = list(
    theta = NULL
  )
)
