library(R6)
#' @export
BasicAgent <- R6Class(
  "BasicAgent",
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
      private$memory$theta =         rep(0, self$bandit$k)          # or self$values.. or below under theta
      private$memory$choice.counts = rep(0, self$bandit$k)          # per arm count
      private$memory$succes.counts = rep(0, self$bandit$k)          # per arm succesful count
      private$memory$current.choice = list()                        # current arm choice
    },
    get_action = function(context = NULL) {
      private$memory$current.choice = self$policy$get_action(self)
      return(private$memory$current.choice)
    },
    set_reward = function(reward) {
      current.choice = private$memory$current.choice
      inc(private$memory$choice.counts[current.choice]) <- 1
      if (reward == 1) {
        inc(private$memory$succes.counts[current.choice]) <- 1
      }
      current_value_arm = private$memory$theta[current.choice]
      current_prob_arm  = 1 / private$memory$choice.counts[current.choice]
      private$memory$theta[current.choice] = current_prob_arm * (reward - current_value_arm) + current_value_arm
    }
  ),
  private = list(
    memory = NULL
  )
)
