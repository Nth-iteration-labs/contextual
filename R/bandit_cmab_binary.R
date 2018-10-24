 #' @export
ContextualBinaryBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    weights = NULL,
    class_name = "ContextualBinaryBandit",
    initialize = function(weights) {
      self$weights     <- weights
      self$d           <- nrow(weights)
      self$k           <- ncol(weights)
    },
    get_context = function(t) {
      # self$d random features on (1) or off (0)
      Xa <- sample(c(0,1), self$d, replace=TRUE)
      # make sure at least one feature on (1)
      Xa[sample(1:self$d,1)] <- 1
      context <- list(
        X = Xa,
        k = self$k,
        d = self$d
      )
    },
    get_reward = function(t, context, action) {
      arm            <- action$choice
      Xa             <- context$X
      average_weight <- Xa %*% self$weights / sum(Xa)
      rewards        <- as.double(average_weight > runif(self$k))
      optimal_arm    <- which_max_tied(rewards)
      reward  <- list(
        reward                   = rewards[arm],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)

#' Bandit: ContextualBinaryBandit
#'
#' Contextual Bernoulli multi-armed bandit with at least one context feature active at a time.
#'
#' @name ContextualBinaryBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- ContextualBinaryBandit$new(weights)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{weights}}{
#'      numeric matrix; \code{d x k} matrix with probabilities of reward for \code{d} contextual features
#'      per \code{k} arms
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(weights)}}{ generates and initializes a new \code{ContextualBinaryBandit}
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
#' Bandit subclass examples: \code{\link{ContextualBinaryBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' horizon            <- 100
#' sims               <- 100
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'
#' bandit             <- ContextualBinaryBandit$new(weights = c(0.6, 0.1, 0.1))
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(agent, horizon, sims)$run()
#'
#' plot(history, type = "cumulative", regret = TRUE)
#'
#' }
NULL


