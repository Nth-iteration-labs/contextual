library(R6)
#' @export
Agent <- R6Class(
  "Agent",
  portable = FALSE, class = FALSE, cloneable = TRUE,
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(
      policy,
      bandit
    ) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    get_memory = function() {
      return(private$memory)
    },
    reset = function() {
      self$bandit$reset()
      private$memory$theta = rep(0, self$bandit$k)
      private$memory$n_arm = rep(0, self$bandit$k)
      private$memory$action = list()
    },
    get_action = function(context = NULL) {
      action = self$policy$get_action(self)
      private$memory$action = action
      return(action)
    },
    set_reward = function(reward) {
      action = private$memory$action
      private$memory$n_arm[action] = private$memory$n_arm[action] + 1
      current_value_arm = private$memory$theta[action]
      current_p_arm  = 1 / private$memory$n_arm[action]
      private$memory$theta[action] = current_value_arm + current_p_arm * (reward - current_value_arm)
    }
  ),
  private = list(
    memory = NULL
  )
)
