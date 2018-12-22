#' @export
UCB2Policy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UCB2Policy",
    alpha = NULL,
    current_arm = NULL,
    next_update = NULL,
    initialize = function(alpha = 0.1) {
      super$initialize()
      self$alpha       <- alpha
      self$current_arm <- 0
      self$next_update <- 0
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('n' = 0, 'mean' = 0, 'r' = 0)
    },
    get_action = function(t, context) {
      n_zero_arms <- which(self$theta$n == 0)
      if (length(n_zero_arms) > 0) {
        action$choice <- sample_one_of(n_zero_arms)
        set_arm(action$choice)
        return(action)
      }

      # make sure we aren't still playing the previous arm.
      if (self$next_update > sum_of(self$theta$n)) {
        action$choice <- self$current_arm
        return(action)
      }

      expected_rewards <- rep(0.0, context$k)
      total_n          <- sum_of(self$theta$n)

      for (arm in 1:context$k) {
        bonus                 <- private$bonus(total_n, self$theta$mean[[arm]])
        expected_rewards[arm] <- self$theta$mean[[arm]] + bonus
      }

      action$choice <- which_max_tied(expected_rewards)
      private$set_arm(action$choice)

      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$n[[arm]]) <- 1
      inc(self$theta$mean[[arm]]) <- (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      self$theta
    }
  ),
  private = list(
    bonus = function(n, r){
      tau   <- private$tau(r)
      bonus <- sqrt((1 + self$alpha) * log(exp(1) * n / tau) / (2 * tau))
    },
    tau = function(r){
      ceiling((1 + self$alpha) ^ r)
    },
    set_arm = function(arm){
      # When choosing a new arm, make sure we play that arm for tau(r+1) - tau(r) episodes.
      self$current_arm         <- arm
      r_arm                    <- self$theta$r[[arm]]
      inc(self$next_update)    <- max(1, private$tau(r_arm + 1) - private$tau(r_arm))
      inc(self$theta$r[[arm]]) <- 1
    }
  )
)

#' Policy: UCB2
#'
#' UCB policy for bounded bandits with plays divided in epochs.
#'
#' \code{UCB2Policy} constructs an optimistic estimate in the form of an Upper Confidence Bound to
#' create an estimate of the expected payoff of each action, and picks the action with the highest estimate.
#' If the guess is wrong, the optimistic guess quickly decreases, till another action has
#' the higher estimate.
#'
#' @name UCB2Policy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- UCB2Policy(alpha = 0.1)
#' }
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{alpha}}{
#'    numeric; Tuning parameter in the interval \code{(0,1)}
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(alpha = 0.1)}}{ Generates a new \code{UCB2Policy} object.}
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
#' @references
#'
#' Auer, P., Cesa-Bianchi, N., & Fischer, P. (2002). Finite-time analysis of the multiarmed bandit problem.
#' Machine learning, 47(2-3), 235-256.
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
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights          <- c(0.9, 0.1, 0.1)
#'
#' policy             <- UCB2Policy$new()
#' bandit             <- BasicBernoulliBandit$new(weights = weights)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#' plot(history, type = "arms")
NULL
