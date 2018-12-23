#' @export
ContextualEpsilonGreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    class_name = "ContextualEpsilonGreedyPolicy",
    initialize = function(epsilon = 1.0) {
      super$initialize()
      self$epsilon <- epsilon
    },
    set_parameters = function(context_params) {
      d <- context_params$d
      self$theta_to_arms <- list('A' = diag(1,d,d), 'b' = rep(0,d))
    },
    get_action = function(t, context) {
      if (runif(1) > self$epsilon) {
        expected_rewards <- rep(0.0, context$k)
        for (arm in 1:context$k) {
          Xa         <- get_arm_context(context$X, arm)
          A          <- self$theta$A[[arm]]
          b          <- self$theta$b[[arm]]
          A_inv      <- inv(A)
          theta_hat  <- A_inv %*% b
          expected_rewards[arm] <- Xa %*% theta_hat
        }
        action$choice  <- which_max_tied(expected_rewards)
      } else {
        self$action$choice        <- sample.int(context$k, 1, replace = TRUE)
      }

      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      Xa     <- get_arm_context(context$X, arm)

      inc(self$theta$A[[arm]]) <- outer(Xa, Xa)
      inc(self$theta$b[[arm]]) <- reward * Xa

      self$theta
    }
  )
)

#' Policy: ContextualEpsilonGreedyPolicy with unique linear models
#'
#' @name ContextualEpsilonGreedyPolicy
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualEpsilonGreedyPolicy(epsilon = 0.1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    double, a positive real value R+
#'   }
#' }
#'
#' @section Parameters:
#'
#' \describe{
#'   \item{\code{A}}{
#'    d*d identity matrix
#'   }
#'   \item{\code{b}}{
#'    a zero vector of length d
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(epsilon = 0.1)}}{ Generates a new \code{ContextualEpsilonGreedyPolicy} object.
#'   Arguments are defined in the Argument section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters()}}{each policy needs to assign the parameters it wants to keep track of
#'   to list \code{self$theta_to_arms} that has to be defined in \code{set_parameters()}'s body.
#'   The parameters defined here can later be accessed by arm index in the following way:
#'   \code{theta[[index_of_arm]]$parameter_name}
#'   }
#' }
#'
#' \describe{
#'   \item{\code{get_action(context)}}{
#'     here, a policy decides which arm to choose, based on the current values
#'     of its parameters and, potentially, the current context.
#'    }
#'   }
#'
#'  \describe{
#'   \item{\code{set_reward(reward, context)}}{
#'     in \code{set_reward(reward, context)}, a policy updates its parameter values
#'     based on the reward received, and, potentially, the current context.
#'    }
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
NULL
