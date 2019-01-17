#' @export
ContextualLinTSPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    sigma = NULL,
    class_name = "ContextualLinTSPolicy",
    initialize = function(v = 0.2) {
      super$initialize()
      self$sigma   <- v^2
    },
    set_parameters = function(context_params) {
      ul                 <- length(context_params$unique)
      self$theta_to_arms <- list('A_inv' = diag(1, ul, ul), 'b' = rep(0, ul))
    },
    get_action = function(t, context) {
      expected_rewards           <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        Xa                       <- get_arm_context(context$X, arm, context$unique)
        A_inv                    <- self$theta$A_inv[[arm]]
        b                        <- self$theta$b[[arm]]

        theta_hat                <- A_inv %*% b
        sigma_hat                <- self$sigma * A_inv
        theta_tilde              <- as.vector(contextual::mvrnorm(1, theta_hat, sigma_hat))
        expected_rewards[arm]    <- Xa %*% theta_tilde
      }
      action$choice              <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward

      Xa    <- get_arm_context(context$X, arm, context$unique)

      self$theta$A_inv[[arm]]  <- sherman_morrisson(self$theta$A_inv[[arm]],Xa)
      self$theta$b[[arm]]      <- self$theta$b[[arm]] + reward * Xa

      self$theta
    }
  )
)

#' Policy: Linear Thompson Sampling with unique linear models
#'
#' \code{ContextualLinTSPolicy} implements Thompson Sampling with Linear
#' Payoffs, following Agrawal and Goyal (2011).
#' Thompson Sampling with Linear Payoffs is a contextual Thompson Sampling multi-armed bandit
#' Policy which assumes the underlying relationship between rewards and contexts
#' are linear. Check the reference for more details.
#'
#' @name ContextualLinTSPolicy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualLinTSPolicy$new(v = 0.2)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{v}}{
#'    double, a positive real value R+;
#'    Hyper-parameter for adjusting the variance of posterior gaussian distribution.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(v)}}{ instantiates a new \code{ContextualLinTSPolicy} instance.
#'      Arguments defined in the Arguments section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters(context_params)}}{
#'      initialization of policy parameters, utilising \code{context_params$k} (number of arms) and
#'      \code{context_params$d} (number of context features).
#'   }
#' }
#'
#' \describe{
#'   \item{\code{get_action(t,context)}}{
#'     selects an arm based on \code{self$theta} and \code{context}, returning the index of the selected arm
#'     in \code{action$choice}. The {context} argument consists of a list with \code{context$k} (number of arms),
#'     \code{context$d} (number of features), and the feature matrix \code{context$X} with dimensions
#'     \eqn{d \times k}{d x k}.
#'    }
#'   }
#'
#'  \describe{
#'   \item{\code{set_reward(t, context, action, reward)}}{
#'     updates parameter list \code{theta} in accordance with the current \code{reward$reward},
#'     \code{action$choice} and the feature matrix \code{context$X} with dimensions
#'     \eqn{d \times k}{d x k}. Returns the updated \code{theta}.
#'    }
#'   }
#'
#' @references
#'
#' Shipra Agrawal, and Navin Goyal. "Thompson Sampling for Contextual Bandits with Linear Payoffs." Advances
#' in Neural Information Processing Systems 24. 2011.
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
#' \dontrun{
#'
#' horizon       <- 100L
#' simulations   <- 100L
#'
#' bandit        <- ContextualLinearBandit$new(k = 4, d = 3, sigma = 0.3)
#'
#' agents        <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
#'                       Agent$new(ContextualLinTSPolicyPolicy$new(0.1), bandit, "LinTSPolicy"))
#'
#' simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
#'
#' history        <- simulation$run()
#'
#' plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")
#'
#' }
NULL


