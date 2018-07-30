#' @export
OraclePolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "OraclePolicy",
    initialize = function() {
      super$initialize()
    },
    set_parameters = function(k, d, u, s) {
      self$theta_to_arms          <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(t, context) {
      action$propensity           <- 1
      action$choice               <- context$o
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


#' Policy: Oracle
#'
#' \code{OraclePolicy} is a also known as a "cheating" or "godlike"
#' policy, as it knows the reward probabilities at all times,
#' and will always play the optimal arm. It is often used as
#' a baseline to compare other policies to.
#'
#' @name OraclePolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- OraclePolicy()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new()}}{ Generates a new \code{OraclePolicy} object. Arguments are defined in the Argument section above.}
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
#' Gittins, J., Glazebrook, K., & Weber, R. (2011). Multi-armed bandit allocation indices. John Wiley & Sons. (Original work published 1989)
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#'
NULL
