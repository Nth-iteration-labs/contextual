#' @export
RandomPolicy <- R6::R6Class(
  "RandomPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    initialize = function(name = "Random") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$theta_to_arms <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context, t) {
      action$choice <- sample.int(context$k, 1, replace = TRUE)
      action$propensity <- 1/context$k
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      reward <- reward$reward
      inc(theta$n[[arm]]) <- 1
      inc(theta$mean[[arm]]) <- (reward - theta$mean[[arm]]) / theta$n[[arm]]
      theta
    }
  )
)

#' Policy: Random
#'
#' \code{RandomPolicy} always explores, choosing arms uniformly at random.
#' In that respect, \code{RandomPolicy} is the mirror image of a pure greedy policy,
#' which would always seek to exploit.
#'
#' @name RandomPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- RandomPolicy(name = "RandomPolicy")
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
#'   \item{\code{new(name = "Random")}}{ Generates a new \code{RandomPolicy} object. Arguments are defined in the Argument section above.}
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
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBandit}},
#' \code{\link{RejectionSamplingOfflineBandit}}, \code{\link{SyntheticBandit}}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights        <- c(0.9, 0.1, 0.1)
#'
#' policy             <- RandomPolicy$new(name = "Random")
#' bandit             <- SyntheticBandit$new(weights = weights, precaching = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "grid")
#'
#'
NULL
