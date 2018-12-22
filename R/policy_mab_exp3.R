#' @export
Exp3Policy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    gamma = NULL,
    class_name = "Exp3Policy",
    initialize = function(gamma =  0.1) {
      super$initialize()
      self$gamma <- gamma
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('weight' = 1)
    },
    get_action = function(t, context) {
      probs <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (self$theta$weight[[i]] / sum_of(self$theta$weight))
      }
      inc(probs[i])  <- ((gamma) * (1.0 / context$k))
      action$choice  <- categorical_draw(probs)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      probs  <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (self$theta$weight[[i]] / sum_of(self$theta$weight))
         inc(probs[i]) <- gamma  / context$k
      }
      growth_factor <- exp((gamma / context$k) * reward / probs[arm])
      self$theta$weight[[arm]] <- self$theta$weight[[arm]] * growth_factor
      self$theta
    },
    categorical_draw = function(probs) {
      arms <- length(probs)
      cumulative_probability <- 0.0
      for (i in 1:arms) {
        inc(cumulative_probability) <- probs[i]
        if ( cumulative_probability > runif(1) ) return(i)
      }
      sample(arms, 1, replace = TRUE)
    }
  )
)

#' Policy: Exp3
#'
#' In \code{Exp3Policy}, "Exp3" stands for "Exponential-weight algorithm for Exploration and Exploitation".
#' It makes use of a distribution over probabilities that is is a mixture of a
#' uniform distribution and a distribution which assigns to each action
#' a probability mass exponential in the estimated cumulative reward for that action.
#'
#' @name Exp3Policy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- Exp3Policy(gamma = 0.1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{gamma}}{
#'    double, value in the closed interval \code{(0,1]}, controls the exploration - often referred to as the learning rate
#'   }
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, among others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(gamma = 0.1)}}{ Generates a new \code{Exp3Policy} object. Arguments are defined in the Argument section above.}
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
#' Auer, P., Cesa-Bianchi, N., Freund, Y., & Schapire, R. E. (2002). The nonstochastic multi-armed bandit
#' problem. SIAM journal on computing, 32(1), 48-77. Strehl, A., & Littman, M. (2004). Exploration via
#' model based interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' Strehl, A., & Littman, M. (2004). Exploration via model based interval estimation. In International
#' Conference on Machine Learning, number Icml.
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
#' policy             <- Exp3Policy$new(gamma = 0.1)
#' bandit             <- BasicBernoulliBandit$new(weights = weights)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#' plot(history, type = "arms")
NULL
