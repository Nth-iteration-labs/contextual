#' @export
EpsilonFirstPolicy              <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    first = NULL,
    class_name = "EpsilonFirstPolicy",
    initialize = function(epsilon = 0.1, N = 1000) {
      super$initialize()
      self$first                <- ceiling(epsilon*N)
    },
    set_parameters = function(context_params) {
      self$theta_to_arms        <- list('n' = 0, 'mean' = 0)

      # Above, we define a list with 'n' and 'mean' theta parameters to each
      # arm through helper variable self$theta_to_arms. That is, when the
      # number of arms is 'k', the above would equal:

      # self$theta <- list(n = rep(list(0,k)), 'mean' = rep(list(0,k)))

      # ... which would also work just fine, but is much less concise.

      # When assigning both to self$theta directly & via self$theta_to_arms,
      # make sure to do it in that particular order.

    },
    get_action = function(t, context) {
      if (sum_of(self$theta$n) < self$first) {
        action$choice           <- sample.int(context$k, 1, replace = TRUE)
        action$propensity       <- (1/context$k)
      } else {
        action$choice           <- which_max_list(self$theta$mean)
        action$propensity       <- 1
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                       <- action$choice
      reward                    <- reward$reward
      inc(self$theta$n[[arm]])  <- 1
      if (sum_of(self$theta$n) < self$first - 1) {
        inc(self$theta$mean[[arm]]) <-
          (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      }
      self$theta
    }
  )
)

#' Policy: Epsilon First
#'
#' \code{EpsilonFirstPolicy} implements a "naive" policy where a pure exploration phase
#' is followed by a pure exploitation phase.
#'
#' Exploration happens within the first \code{epsilon * N} time steps.
#' During this time, at each time step \code{t}, \code{EpsilonFirstPolicy} selects an arm at random.
#'
#' Exploitation happens in the following \code{(1-epsilon) * N} steps,
#' selecting the best arm up until \code{epsilon * N} for either the remaining N trials or horizon T.
#'
#' In case of a tie in the exploitation phase, \code{EpsilonFirstPolicy} randomly selects and arm.
#'
#' @section Algorithm:
#'
#' ![](algoepsilonfirst.jpg "epsilon epsilon algorithm")
#'
#' @name EpsilonFirstPolicy
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonFirstPolicy(epsilon = 0.1, N = 100)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    numeric; value in the closed interval \code{(0,1]} that sets the number of time steps to explore
#'    through \code{epsilon * N}.
#'   }
#'   \item{\code{N}}{
#'    integer; positive integer which sets the number of time steps to explore through \code{epsilon * N}.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(epsilon = 0.1, N = 100)}}{ Generates a new \code{EpsilonFirstPolicy} object. Arguments are defined in the Argument section above.}
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
#' Strehl, A., & Littman, M. (2004). Exploration via model based interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights          <- c(0.9, 0.1, 0.1)
#'
#' policy             <- EpsilonFirstPolicy$new(epsilon = 0.5)
#' bandit             <- BasicBernoulliBandit$new(weights = weights)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#' plot(history, type = "arms")
NULL
