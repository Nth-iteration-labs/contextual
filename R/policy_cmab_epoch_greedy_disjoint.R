#' @export
ContextualEpochGreedyDisjointPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    p = NULL,
    e = NULL,
    class_name = "ContextualEpochGreedyDisjointPolicy",
    initialize = function(p = 10) {
      super$initialize()
      self$p <- p
      self$e <- 0
    },
    set_parameters = function(context_params) {
      ul <- length(context_params$unique)
      self$theta_to_arms <- list( 'A' = diag(1,ul,ul), 'b' = rep(0,ul), 'n' = 0)
    },
    get_action = function(t, context) {
      if (t <= self$p || t <= context$k) {
        arm <- 1 + (t %% context$k)
        self$action$choice = arm
        return(self$action)
      }
      self$e <- rbinom(1,1,self$p/t)
      if(self$e==1) {
          arm <- sample.int(context$k, 1, replace = TRUE)
          self$action$choice = arm

          return(self$action)
      } else {
        expected_rewards <- rep(0.0, context$k)
        for (arm in 1:context$k) {
          X                     <- context$X[context$unique,arm]
          A                     <- self$theta$A[[arm]]
          b                     <- self$theta$b[[arm]]
          A_inv                 <- inv(A)
          theta_hat             <- A_inv %*% b
          mean                  <- X %*% theta_hat
          expected_rewards[arm] <- mean / sqrt(self$theta$n[[arm]])
        }
        self$action$choice  <- max_in(expected_rewards)
        return(self$action)
      }
    },
    set_reward = function(t, context, action, reward) {
      if (t <= self$p || t <= context$k || self$e==1) {
        arm                      <- action$choice
        reward                   <- reward$reward
        Xa                       <- context$X[context$unique, arm]
        self$theta$n[[arm]]      <- self$theta$n[[arm]] + 1
        inc(self$theta$A[[arm]]) <- outer(Xa, Xa)
        inc(self$theta$b[[arm]]) <- reward * Xa
      }
      self$theta
    }
  )
)
#' Policy: A Time and Space Efficient Algorithm for Contextual Linear Bandits
#'
#' @name ContextualEpochGreedyDisjointPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonGreedyPolicy(epsilon = 0.1)
#' }
#'
#'
NULL
