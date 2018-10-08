#' @export
ContextualBernoulliPrecachingBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    weights       = NULL,
    class_name    = "ContextualBernoulliPrecachingBandit",
    initialize    = function(weights) {
      if (is.vector(weights)) {
        self$weights <- matrix(weights, nrow = 1L)
      } else {
        self$weights <- weights
      }
      self$d <- nrow(self$weights)
      self$k <- ncol(self$weights)
    },
    get_context = function(t) {
      context <- list(k = self$k, d = self$d, X = private$X[,, t])
    },
    get_reward = function(t, context, action) {
      reward <- list(
        reward         = private$R[action$choice, t],
        optimal_reward = as.double(private$R[which_max_tied(private$R[, t]), t]),
        optimal_arm    = which_max_tied(private$R[, t])
      )
    },
    generate_bandit_data = function(n = 1L) {
      private$generate_contexts(n)
      private$generate_rewards(n)
    }
  ),
  private = list(
    R = NULL,
    X = NULL,
    context_list = list(),
    reward_list = list(),
    generate_contexts = function(n = 1L) {
      private$X <- array(sample(c(0, 1), replace = TRUE, size = self$d * n), dim = c(self$d, self$k, n))
    },
    generate_rewards = function(n) {
      weight_array                 <- array(t(matrix(self$weights, self$k , self$d, byrow = TRUE)),
                                            dim = c(self$d, self$k, n))
      rewards                      <- colSums(private$X*weight_array)
      rewards                      <- rewards / colSums(private$X)
      rewards[is.nan(rewards)]     <- 0
      private$R <- round((runif( self$k * n) + rewards) / 2)
    }
  )
)

#' Bandit: ContextualBernoulliPrecachingBandit
#'
#' Illustrates precaching of contexts and rewards.
#'
#' Contextual extension of \code{\link{BasicBernoulliBandit}}.
#'
#' Contextual extension of \code{\link{BasicBernoulliBandit}} where a user specified \code{d x k} dimensional
#' matrix takes the place of \code{\link{BasicBernoulliBandit}} \code{k} dimensional probability vector. Here,
#' each row \code{d} represents a feature with \code{k} reward probability values per arm.
#'
#' For every \code{t}, \code{ContextualBernoulliPrecachingBandit} randomly samples from its \code{d} features/rows at
#' random, yielding a binary \code{context} matrix representing sampled (all 1 rows) and unsampled (all 0)
#' features/rows. Next, \code{ContextualBernoulliPrecachingBandit} generates \code{rewards} contingent on either sum or
#' mean (default) probabilities of each arm/column over all of the sampled features/rows.
#'
#' @name ContextualBernoulliPrecachingBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- ContextualBernoulliPrecachingBandit$new(weights)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{weights}}{
#'      numeric matrix; \code{d x k} dimensional matrix where each row \code{d} represents a feature with
#'      \code{k} reward probability values per arm.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(weights)}}{ generates
#'   and instantializes a new \code{ContextualBernoulliPrecachingBandit} instance. }
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
#'
#'   \item{\code{generate_bandit_data()}}{
#'      helper function called before \code{Simulator} starts iterating over all time steps \code{t} in T.
#'      Pregenerates \code{contexts} and \code{rewards}.
#'  }
#' }
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
#' \dontrun{
#'
#' horizon            <- 100L
#' simulations        <- 100L
#'
#' # rows represent features, columns represent arms:
#'
#' context_weights    <- matrix(  c(0.4, 0.2, 0.4,
#'                                  0.3, 0.4, 0.3,
#'                                  0.1, 0.8, 0.1),  nrow = 3, ncol = 3, byrow = TRUE)
#'
#' bandit             <- ContextualBernoulliPrecachingBandit$new(weights)
#'
#' agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#'                             Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation         <- Simulator$new(agents, horizon, simulations)
#' history            <- simulation$run()
#'
#' plot(history, type = "cumulative")
#'
#' }
NULL

