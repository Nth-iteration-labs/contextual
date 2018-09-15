 #' @export
BasicBernoulliBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    p_per_arm = NULL,
    class_name = "BasicBernoulliBandit",
    initialize = function(p_per_arm) {
      self$p_per_arm   <- p_per_arm
      self$k           <- length(self$p_per_arm)
    },
    get_context = function(t) {
      context <- list(
        k = self$k
      )
    },
    get_reward = function(t, context, action) {
      rewards        <- as.double(runif(self$k) < self$p_per_arm)
      optimal_arm    <- which.max(rewards)
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
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- BasicBernoulliBandit$new(p_per_arm)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{p_per_arm}}{
#'      numeric vector; probability of reward values for each of the bandit's \code{k} arms
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(p_per_arm)}}{
#'      generates and instantializes a new \code{Bandit} instance.
#'      For arguments, see Argument section above.
#'   }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the number of arms as \code{context$k}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, with \code{context$k} (number of arms).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward}
#'  }
#
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
#'
#' horizon            <- 100
#' sims               <- 100
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'
#' bandit             <- BasicBernoulliBandit$new(p_per_arm = c(0.6, 0.1, 0.1))
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(agent, horizon, sims)$run()
#'
#' plot(history, type = "cumulative", regret = TRUE)
#'
#' }
NULL


