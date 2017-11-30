library(R6)
#' @export
LinUCBAgent <- R6Class(
  "LinUCBAgent",
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
      private$memory$theta = list()                    # instantiate memory for theta
      theta.arm = list(
        'A' = diag(1,bandit$d),                        # A is a d*d identity matrix
        'b' = rep(0,bandit$d)                          # b is a 0 vector of length
      )
      for (i in 1:self$bandit$k) {
        private$memory$theta[[i]] = theta.arm          # assign per arm to theta in memory
      }
    },
    get.action = function(context) {
      return(self$policy$get.action(self,context))
    },
    set.reward = function(reward,context) {
      X = as.vector(context$X)
      inc(private$memory$theta[[reward$current.choice]]$A) <- outer(X, X)
      inc(private$memory$theta[[reward$current.choice]]$b) <- reward$reward * X
    }
  ),
  private = list(
    memory = list()
  )
)
