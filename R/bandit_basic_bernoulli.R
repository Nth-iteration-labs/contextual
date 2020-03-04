#' @export
BasicBernoulliBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    weights = NULL,
    class_name = "BasicBernoulliBandit",
    initialize = function(weights) {
      self$weights     <- weights
      self$k           <- length(self$weights)
    },
    get_context = function(t) {
      context <- list(
        k = self$k
      )
    },
    get_reward = function(t, context, action) {
      rewards        <- as.double(runif(self$k) < self$weights)
      optimal_arm    <- which_max_tied(self$weights)
      reward  <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)

#' Bandit: BasicBernoulliBandit
#'
#' Context-free Bernoulli or Binary multi-armed bandit.
#'
#' Simulates \code{k} Bernoulli arms where each arm issues a reward of one with
#' uniform probability \code{p}, and otherwise a reward of zero.
#'
#' In a bandit scenario, this can be used to simulate a hit or miss event,
#' such as if a user clicks on a headline, ad, or recommended product.
#'
#' @name BasicBernoulliBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- BasicBernoulliBandit$new(weights)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{weights}}{
#'      numeric vector; probability of reward values for each of the bandit's \code{k} arms
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(weights)}}{ generates and instantializes a new \code{BasicBernoulliBandit}
#'    instance. }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the current \code{d x k} dimensional matrix \code{context$X},
#'      the number of arms \code{context$k} and the number of features \code{context$d}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward} and, where computable,
#'         \code{reward$optimal} (used by "oracle" policies and to calculate regret).
#'  }
#
#' }
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
#' horizon            <- 100
#' sims               <- 100
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'
#' bandit             <- BasicBernoulliBandit$new(weights = c(0.6, 0.1, 0.1))
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(agent, horizon, sims)$run()
#'
#' plot(history, type = "cumulative", regret = TRUE)
#'
#' }
NULL


