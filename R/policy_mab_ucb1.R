#' @export
UCB1Policy <- R6::R6Class(
  "UCB1Policy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    initialize = function(name = "UCB1") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$theta_to_arms <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(t, context) {
      n_zero_arms <- which(self$theta$n == 0)
      if (length(n_zero_arms) > 0) {
        action$choice <- sample_one_of(n_zero_arms)
        return(action)
      }
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        variance <- sqrt( (2*log(sum_of(self$theta$n))) / self$theta$n[[arm]])
        expected_rewards[arm] <- self$theta$mean[[arm]] + variance
      }
      action$choice <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$n[[arm]]) <- 1
      inc(self$theta$mean[[arm]]) <- (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      self$theta
    }
  )
)

#' Policy: UCB1  ..
#'
#' \code{UCB1Policy} chooses an arm at
#' random (explores) with probability \code{epsilon}, otherwise it
#' greedily chooses (exploits) the arm with the highest estimated
#' reward.
#'
#' @name UCB1Policy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- UCB1Policy(epsilon = 0.1, name = "UCB1Policy")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    double, value in the closed interval \code{(0,1]} indicating the probablilty with which
#'    arms are selected at random (explored).
#'    Otherwise, \code{UCB1Policy} chooses the best arm (exploits)
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
#'   \item{\code{new(epsilon = 0.1, name = "EpsilonGreedy")}}{ Generates a new \code{UCB1Policy} object. Arguments are defined in the Argument section above.}
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
#' Sutton, R. S. (1996). Generalization in reinforcement learning: Successful examples using sparse coarse coding. In Advances in neural information processing systems (pp. 1038-1044).
#'
#' Strehl, A., & Littman, M. (2004). Exploration via modelbased interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights            <- c(0.9, 0.1, 0.1)
#'
#' policy             <- UCB1Policy$new(name = "UCB1")
#' bandit             <- SyntheticBandit$new(weights = weights, precaching = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#' plot(history, type = "arms")
#'
#'
NULL
