library(R6)

#' @export
LinUCBAgent <- R6Class(
  "LinUCBAgent",
  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(
      policy = NA,
      bandit = NA
    ) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    reset = function() {
      private$memory[['theta']] = list()                    # instantiate memory for theta
      theta_arm = list(
        'A' = diag(1,bandit$d),                             # A is a d*d identity matrix
        'b' = vector(mode = "numeric", length = bandit$d)   # b is a 0 vector of length d    ---- turned!
      )
      for (i in 1:bandit$k) {
        private$memory[['theta']][[i]] = theta_arm          # assign per arm to theta in memory
      }
    },
    get_action = function(context) {
      action = self$policy$get_action(self,context)
      private$memory[['action']] = action
      private$memory[['context']] = context
      return(action)
    },
    get_memory = function() {
        return(private$memory)
    },
    set_reward = function(reward) {

      A_chosen_arm = private$memory[['theta']][[private$memory[['action']]]][['A']]  # or lastaction, as in orig agent
      b_chosen_arm = private$memory[['theta']][[private$memory[['action']]]][['b']]
      mem = as.vector(private$memory[['context']])

      private$memory[['theta']][[private$memory[['action']]]][['A']] = A_chosen_arm + outer(mem, mem)
      private$memory[['theta']][[private$memory[['action']]]][['b']] = as.matrix(b_chosen_arm) + reward * mem

    }
  ),
  private = list(
    memory = list()
  )
)
