#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    epsilon = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      super$initialize(name)
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      if (runif(1) < epsilon) {
        action$choice       <- sample.int(context$k, 1, replace = TRUE)
        action$propensity   <- epsilon*(1/context$k)
      } else {
        action$choice       <- max_in(theta$mean)
        action$propensity   <- 1 - epsilon
      }
      action
    },
    set_reward = function(reward, context) {

      arm <- reward$choice ; reward <- reward$reward

      inc(theta$n[[arm]])    <- 1
      inc(theta$mean[[arm]]) <- (reward - theta$mean[[arm]]) / theta$n[[arm]]

      theta

    }
  )
)

# think about rmax if not boolean!!! Rmax.reward!!!?


#' Policy: Epsilon Greedy
#'
#' \code{EpsilonGreedyPolicy} chooses an arm at
#' random (explores) with probability \code{epsilon}, otherwise it
#' greedily chooses the arm with the highest estimated
#' reward (exploits). To make sure that all arms receive at least one pull, arms
#' that have not yet been chosen are given an estimate of Rmax.
#'
#' @name EpsilonGreedyPolicy
#' @family contextual classes
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonGreedyPolicy(epsilon = 0.1, name = "EpsilonGreedyPolicy")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    double, value in the closed interval \code{[0,1]} indicating the probablilty with which
#'    arms are selected at random (explored).
#'    Otherwise, \code{EpsilonGreedyPolicy} chooses the best arm (exploits)
#'    with a probability of \code{1 - epsilon}
#'
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
#'   \item{\code{new(epsilon = 0.1, name = "EpsilonGreedy")}}{ Generates a new \code{EpsilonGreedyPolicy} object. Arguments are defined in the Argument section above.}
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
#' Online: \code{\href{https://nth-iteration-labs.github.io/contextual/index.html}{Documentation}}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weight_per_arm     <- c(0.9, 0.1, 0.1)
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1, name = "EpsilonGreedy")
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
