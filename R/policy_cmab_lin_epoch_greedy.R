#' @export
ContextualEpochGreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    sZl = NULL,
    exploration_phase = NULL,
    class_name = "ContextualEpochGreedyPolicy",
    initialize = function(sZl = 10) {
      super$initialize()
      self$sZl <- sZl
    },
    set_parameters = function(context_params) {
      d <- context_params$d
      self$theta_to_arms <- list('A' = diag(1,d,d), 'b' = rep(0,d))
    },
    get_action = function(t, context) {

      if(t==1 || t%%self$sZl==0) self$exploration_phase = TRUE

      if (!isTRUE(self$exploration_phase)) {
        expected_rewards <- rep(0.0, context$k)
        for (arm in 1:context$k) {
          Xa         <- get_arm_context(context, arm)
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
      Xa     <- get_arm_context(context, arm)

      if (isTRUE(self$exploration_phase)) {
        inc(self$theta$A[[arm]]) <- outer(Xa, Xa)
        inc(self$theta$b[[arm]]) <- reward * Xa
        self$exploration_phase   <- FALSE
      }

      self$theta
    }
  )
)
#' Policy: A Time and Space Efficient Algorithm for Contextual Linear Bandits
#'
#' @name ContextualEpochGreedyPolicy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonGreedyPolicy(epsilon = 0.1)
#' }
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
