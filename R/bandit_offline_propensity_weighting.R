#' @export
#' @import Formula
OfflinePropensityWeightingBandit <- R6::R6Class(
  inherit = OfflineBootstrappedReplayBandit,
  class = FALSE,
  private = list(
    p = NULL,
    marginal_prob = NULL
  ),
  public = list(
    class_name = "OfflinePropensityWeightingBandit",
    stabilize = NULL,
    preweighted = NULL,
    initialize   = function(formula,
                            data, k = NULL, d = NULL,
                            unique = NULL, shared = NULL,
                            randomize = TRUE, replacement = FALSE,
                            jitter = FALSE, arm_multiply = FALSE,
                            stabilize = TRUE, preweighted = TRUE) {

      self$preweighted <- preweighted # Has the propensity column been preweighted?
      self$stabilize   <- stabilize   # Whether or not to stabilize the weights.
      super$initialize(formula,
                       data, k, d,
                       unique, shared,
                       randomize, replacement,
                       jitter, arm_multiply)
    },
    post_initialization = function() {
      super$post_initialization()
      private$p <-  Formula::model.part(private$formula, data = private$S, lhs = 0, rhs = 3, drop = TRUE)
      if (self$stabilize) {
        private$marginal_prob <- table(private$z)/length(private$z)
      } else {
        private$marginal_prob <- rep(1,self$k)
      }

    },
    get_context = function(index) {
      context <- list(
        k      = self$k,
        d      = self$d,
        unique = self$unique,
        shared = self$shared,
        X      = if(isTRUE(self$flat_context)) private$x[index,] else matrix(private$x[index,],self$d,self$k)
      )
      context
    },
    get_reward = function(index, context, action) {
      p <- private$p[index]
      if (self$preweighted) {
        p <- p * (private$marginal_prob[action$choice])
      } else {
        p <- (1 / p) * (private$marginal_prob[action$choice])
      }
      if (private$z[[index]] == action$choice) {
        list(
          reward         = as.double(private$y[index]*p),
          optimal_reward = ifelse(private$or, as.double(private$S$optimal_reward[[index]]), NA),
          optimal_arm    = ifelse(private$oa, as.double(private$S$optimal_arm[[index]]), NA)
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Offline Propensity Weighted Replay
#'
#' Policy for the evaluation of policies with offline data through replay with propensity weighting.
#'
#' @name OfflinePropensityWeightingBandit
#'
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflinePropensityWeightingBandit(formula,
#'                                              data, k = NULL, d = NULL,
#'                                              unique = NULL, shared = NULL,
#'                                              randomize = TRUE, replacement = TRUE,
#'                                              jitter = TRUE, arm_multiply = TRUE,
#'                                              stabilize = TRUE, preweighted = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{formula}}{
#'     formula (required). Format: \code{y.context ~ z.choice | x1.context + x2.xontext + ... | p.propensity }
#'     By default,  adds an intercept to the context model. Exclude the intercept, by adding "0" or "-1" to
#'     the list of contextual features, as in:
#'     \code{y.context ~ z.choice | x1.context + x2.xontext -1 | p.propensity }
#'   }
#'   \item{\code{data}}{
#'     data.table or data.frame; offline data source (required)
#'   }
#'   \item{\code{k}}{
#'     integer; number of arms (optional). Optionally used to reformat the formula defined x.context vector
#'     as a \code{k x d} matrix. When making use of such matrix formatted contexts, you need to define custom
#'     intercept(s) when and where needed in data.table or data.frame.
#'   }
#'   \item{\code{d}}{
#'     integer; number of contextual features (optional) Optionally used to reformat the formula defined
#'     x.context vector as a \code{k x d} matrix. When making use of such matrix formatted contexts, you need
#'     to define custom intercept(s) when and where needed in data.table or data.frame.
#'   }
#'   \item{\code{randomize}}{
#'     logical; randomize rows of data stream per simulation (optional, default: TRUE)
#'   }
#'   \item{\code{replacement}}{
#'     logical; sample with replacement (optional, default: TRUE)
#'   }
#'   \item{\code{replacement}}{
#'     logical; add jitter to contextual features (optional, default: TRUE)
#'   }
#'   \item{\code{arm_multiply}}{
#'     logical; multiply the horizon by the number of arms (optional, default: TRUE)
#'   }
#'   \item{\code{stabilize}}{
#'     logical; stabilize the propensity scores (optional, default: TRUE)
#'   }
#'   \item{\code{preweighted}}{
#'     logical; have the propensity scores been weighted (optional, default: FALSE)
#'   }
#'   \item{\code{unique}}{
#'     integer vector; index of disjoint features (optional)
#'   }
#'   \item{\code{shared}}{
#'     integer vector; index of shared features (optional)
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(formula, data, k = NULL, d = NULL, unique = NULL, shared = NULL, randomize = TRUE,
#'                   replacement = TRUE, jitter = TRUE, arm_multiply = TRUE, stabilize = TRUE,
#'                   preweighted = FALSE)}}{ generates
#'    and instantializes a new \code{OfflinePropensityWeightingBandit} instance. }
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
#'   \item{\code{post_initialization()}}{
#'      Randomize offline data by shuffling the offline data.table before the start of each
#'      individual simulation when self$randomize is TRUE (default)
#'   }
#' }
#'
#' @references
#'
#' Agarwal, Alekh, et al. "Taming the monster: A fast and simple algorithm for contextual bandits."
#' International Conference on Machine Learning. 2014.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflinePropensityWeightingBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
NULL
