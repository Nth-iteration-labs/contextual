#' @export
EpsilonFirstPolicy <- R6::R6Class(
  "EpsilonFirstPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    first = NULL,
    initialize = function(first = 100, name = "EpsilonFirst") {
      super$initialize(name)
      self$first <- first
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      if (sum_of(theta$n) < first) {
        action$choice       <- sample.int(context$k, 1, replace = TRUE)
        action$propensity   <- (1/context$k)
      } else {
        action$choice       <- max_in(theta$mean, equal_is_random = FALSE)
        action$propensity   <- 1
      }
      action
    },
    set_reward = function(reward, context) {
      arm      <- reward$choice
      reward   <- reward$reward

      inc(theta$n[[arm]]) <- 1
      if (sum_of(theta$n) < first - 1)
        inc(theta$mean[[arm]] ) <- (reward - theta$mean[[arm]]) / theta$n[[arm]]

      theta
    }
  )
)

#' Policy: Epsilon First
#'
#' \code{EpsilonFirstPolicy} implements a "naive" policy. That is, \code{EpsilonFirstPolicy}
#' starts out by choosing randomly (exploring) from all available arms for a fixed number of steps.
#' At that point, the \code{EpsilonFirstPolicy} algorithm checks which arm has the highest
#' estimated payoff. From thereon \code{EpsilonFirstPolicy} will only choose that particular
#' arm, never looking back (keep on exploiting that arm for ever).
#'
#' @name EpsilonFirstPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonFirstPolicy(first = 100, name = "EpsilonFirstPolicy")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{first}}{
#'    integer, a natural number N>0 indicating for how many steps \code{EpsilonFirstPolicy} will choose an arm at random,
#'    from thereon settling for the arm that proved to offer the highest reward up till then.
#'   }
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(first = 100, name = "EpsilonFirst")}}{ Generates a new \code{EpsilonFirstPolicy} object. Arguments are defined in the Argument section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters()}}{each policy needs to assign the parameters it wants to keep track of
#'   to list \code{self$parameters} that has to be defined in \code{set_parameters()}'s body.
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
#' Sutton, R. S. (1996). Generalization in reinforcement learning: Successful examples using sparse coarse coding. In Advances in neural information processing systems (pp. 1038-1044).
#'
#' Strehl, A., & Littman, M. (2004). Exploration via modelbased interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{AbstractBandit}}, \code{\link{BasicBandit}},
#' \code{\link{LiLogBandit}}, \code{\link{SyntheticBandit}}
#'
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weight_per_arm     <- c(0.9, 0.1, 0.1)
#'
#' policy             <- EpsilonFirstPolicy$new(first = 50, name = "EpsilonFirst")
#' bandit             <- SyntheticBandit$new(data = weight_per_arm, precache = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "grid")
#'
#' plot(history, type = "arms")
#'
#'
NULL
