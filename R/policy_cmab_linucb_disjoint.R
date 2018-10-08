#' @export
LinUCBDisjointPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "LinUCBDisjointPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha <- alpha
    },
    set_parameters = function(context_params) {
      ul <- length(context_params$unique)
      self$theta_to_arms <- list('A' = diag(1,ul,ul), 'b' = rep(0,ul))
    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, context$k)

      for (arm in 1:context$k) {

        Xa         <- context$X[context$unique,arm]
        A          <- self$theta$A[[arm]]
        b          <- self$theta$b[[arm]]

        A_inv      <- inv(A)

        theta_hat  <- A_inv %*% b

        mean       <- Xa %*% theta_hat
        sd         <- sqrt(tcrossprod(Xa %*% A_inv, Xa))

        expected_rewards[arm] <- mean + self$alpha * sd
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      Xa     <- context$X[, arm]

      inc(self$theta$A[[arm]]) <- outer(Xa, Xa)
      inc(self$theta$b[[arm]]) <- reward * Xa

      self$theta
    }
  )
)

#' Policy: LinUCB with unique linear models
#'
#' Algorithm 1 LinUCB with unique linear models
#' A Contextual-Bandit Approach to
#' Personalized News Article Recommendation
#'
#' Lihong Li et all
#'
#' Each time step t, \code{LinUCBDisjointPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name LinUCBDisjointPolicy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- LinUCBDisjointPolicy(alpha = 1.0)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{alpha}}{
#'    double, a positive real value R+
#'   }
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, among others, saved to the History log and displayed in summaries and plots.
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
#'   \item{\code{new(alpha = 1)}}{ Generates a new \code{LinUCBDisjointPolicy} object. Arguments are defined in the Argument section above.}
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
#' Li, L., Chu, W., Langford, J., & Schapire, R. E. (2010, April). A contextual-bandit approach to personalized news article recommendation. In Proceedings of the 19th international conference on World wide web (pp. 661-670). ACM.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
NULL
