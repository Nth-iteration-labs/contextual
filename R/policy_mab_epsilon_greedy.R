#' @export
EpsilonGreedyPolicy          <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    class_name = "EpsilonGreedyPolicy",
    initialize = function(epsilon = 0.1) {
      super$initialize()
      self$epsilon                <- epsilon
    },
    set_parameters = function(context_params) {
      self$theta_to_arms          <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(t, context) {
      if (runif(1) > self$epsilon) {
        self$action$choice        <- max_in(self$theta$mean)
        self$action$propensity    <- 1 - self$epsilon
      } else {
        self$action$choice        <- sample.int(context$k, 1, replace = TRUE)
        self$action$propensity    <- self$epsilon*(1/context$k)
      }
      self$action
    },
    set_reward = function(t, context, action, reward) {
      arm                         <- action$choice
      reward                      <- reward$reward
      self$theta$n[[arm]]         <- self$theta$n[[arm]] + 1
      self$theta$mean[[arm]]      <- self$theta$mean[[arm]] + (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      self$theta
    }
  )
)

#' Policy: Epsilon Greedy
#'
#' \code{EpsilonGreedyPolicy} chooses an arm at
#' random (explores) with probability \code{epsilon}, otherwise it
#' greedily chooses (exploits) the arm with the highest estimated
#' reward.
#'
#' @name EpsilonGreedyPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonGreedyPolicy(epsilon = 0.1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    double, value in the closed interval \code{(0,1]} indicating the probablilty with which
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
#'   \item{\code{new(epsilon = 0.1)}}{ Generates a new \code{EpsilonGreedyPolicy} object. Arguments are defined in the Argument section above.}
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
#' Yue, Y., Broder, J., Kleinberg, R., & Joachims, T. (2012). The k-armed dueling bandits problem. Journal of Computer and System Sciences, 78(5), 1538-1556.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights            <- c(0.9, 0.1, 0.1)
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#' bandit             <- MabWeightBandit$new(weights = weights)
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
