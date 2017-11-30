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
    get.memory = function() {
      return(private$memory)
    },
    reset = function() {
      private$memory$theta = rep(1.0, self$bandit$k)
    },
    get.action = function(context = NULL) {
      return(self$policy$get.action(self))
    },
    set.reward = function(reward,context = NULL) {
      probs = rep(0.0, self$bandit$k)
      for (arm in 1:self$bandit$k ) {
        probs[arm] = (1 - self$policy$gamma) * (private$memory$theta[arm] / sum(private$memory$theta))
        inc(probs[arm]) <- (self$policy$gamma) * (1.0 / self$bandit$k)
      }
      x = reward$reward / probs[reward$current.choice]
      growth.factor = exp((self$policy$gamma / self$bandit$k) * x)
      mult(private$memory$theta[reward$current.choice]) <- growth.factor
    }
  ),
  private = list(
    memory = NULL
  )
)
