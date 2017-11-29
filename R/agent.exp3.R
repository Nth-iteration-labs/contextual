library(R6)
#' @export
Exp3Agent <- R6Class(
  "Exp3Agent",
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
      private$memory$theta = rep(1.0, self$bandit$k)
      private$memory$current.choice = list()
    },
    get_action = function(context = NULL) {
      private$memory$current.choice = self$policy$get_action(self)
      return(private$memory$current.choice)
    },
    set_reward = function(reward) {
      current.choice = private$memory$current.choice
      probs = rep(0.0, self$bandit$k)
      for (arm in 1:self$bandit$k ) {
        probs[arm] = as.numeric((1 - self$policy$gamma) * (private$memory$theta[arm] / sum(private$memory$theta)))
        probs[arm] = probs[arm] + (self$policy$gamma) * (1.0 / self$bandit$k)
      }
      x = reward / probs[current.choice]
      growth_factor = exp((self$policy$gamma / self$bandit$k) * x)
      private$memory$theta[current.choice] = private$memory$theta[current.choice] * growth_factor
    }
  ),
  private = list(
    memory = NULL
  )
)
