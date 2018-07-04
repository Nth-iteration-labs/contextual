#' @export
ContextualEpsilonGreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    epsilon = NULL,
    class_name = "ContextualEpsilonGreedyPolicy",
    initialize = function(epsilon = 0.1, alpha = 1.0) {
      super$initialize()
      self$epsilon <- epsilon
      self$alpha   <- alpha
    },
    set_parameters = function() {
      self$theta <- list( 'A' = diag(1,self$d,self$d),  'A_inv' = diag(1,self$d,self$d),
                          'b' = rep(0,self$d), 't' = rep(0,self$d))
    },
    get_action = function(t, context) {

      first <- rep(0.0, context$k)



      for (arm in 1:self$k) {
        Xa               <- t(context$X[,arm])
        first[arm]       <- Xa %*% self$theta$A %*% t(Xa)
      }

      first    <- sqrt(first) *  self$alpha
      second   <- t(context$X) %*%  self$theta$t
      rewards  <- first + second
      threshold <- self$epsilon * self$k

      if (runif(1) > threshold) {
        self$action$choice        <- max_in(rewards)
      } else {
        self$action$choice        <- sample.int(context$k, 1, replace = TRUE)
      }

      self$action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      Xa <- context$X[,arm]

      inc(self$theta$A) <- outer(Xa, Xa)
      inc(self$theta$b) <- reward * Xa
      self$theta$A_inv  <- inv(self$theta$A)
      self$theta$t      <- self$theta$A %*% self$theta$b

      self$theta
    }
  )
)

