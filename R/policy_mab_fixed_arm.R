#' @export
FixedPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    fixed_arm = NULL,
    class_name = "FixedPolicy",
    initialize = function(fixed_arm) {
      super$initialize()
      self$fixed_arm <- fixed_arm
    },
    set_parameters = function(context_params) {
      self$theta_to_arms          <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(t, context) {
      action$choice               <- self$fixed_arm
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                         <- action$choice
      reward                      <- reward$reward
      inc(self$theta$n[[arm]])    <- 1
      inc(self$theta$mean[[arm]]) <- (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      self$theta
    }
  )
)
#' Policy: Fixed Arm
#'
#' \code{FixedPolicy} implements a "naive" policy which always chooses a prespecified arm.
#'
#' @name FixedPolicy
#'
#' @section Usage:
#' \preformatted{
#' policy <- FixedPolicy(fixed_arm = 1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{fixed_arm}}{
#'    numeric; index of the arm that will be chosen for each time step.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new()}}{ Generates a new \code{FixedPolicy} object. Arguments are defined in the Argument
#'   section above.}
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
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights            <- c(0.9, 0.1, 0.1)
#'
#' policy             <- FixedPolicy$new(fixed_arm = 2)
#' bandit             <- BasicBernoulliBandit$new(weights = weights)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#' plot(history, type = "arms")
NULL
