#' @export
OfflineDirectMethodBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    oa = NULL,
    or = NULL
  ),
  public = list(
    class_name = "OfflineDirectMethodBandit",
    randomize = NULL,
    arm_regression_models = NULL,
    single_regression_model = NULL,
    initialize   = function(offline_data, k, d = 0, arm_regression_models = NULL, single_regression_model = NULL, randomize = TRUE) {

      self$k <- k                 # Number of arms (integer)
      self$d <- d                 # Dimension of context feature vector (integer)
      self$randomize <-randomize  # Randomize logged events for every simulation? (logical)
      private$S <- offline_data   # Logged events (by default, as a data.table)

      self$arm_regression_models   <- arm_regression_models
      self$single_regression_model <- single_regression_model

      private$S

      private$S[is.null(context[[1]]),`:=`(context = list(1))]
      private$oa <- "optimal_arm" %in% colnames(offline_data)
      private$or <- "optimal_reward" %in% colnames(offline_data)
    },
    post_initialization = function() {
      if(isTRUE(self$randomize)) private$S <- private$S[sample(nrow(private$S))]
    },
    get_context = function(index) {
      context <- list(
        k = self$k,
        d = self$d,
        X = private$S$context[[index]]
      )
      context
    },
    get_reward = function(index, context, action) {

      choice            <- action$choice

      if(!is.null(self$arm_regression_models)) {
        regression_reward <- predict(self$arm_regression_models[[choice]], private$S[index,], type="response")
      }

      if(!is.null(self$single_regression_model)) {
        regression_reward <- predict(self$single_regression_model, private$S[index,], type="response")
      }

      list(
        reward = as.double(regression_reward)
      )
    }
  )
)

#' Bandit: Offline Direct Method Evaluator
#'
#' TODO: Interface needs to be updated, and needs to be documented more fully.
#'
#' @name OfflineDirectMethodBandit
#'
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineDirectMethodBandit(offline_data, k, d = 0, arm_regression_models = NULL,
#'                                       single_regression_model = NULL, randomize = TRUE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{offline_data}}{
#'     data.table; offline data source (required)
#'   }
#'   \item{\code{k}}{
#'     integer; number of arms (required)
#'   }
#'   \item{\code{d}}{
#'     integer; number of contextual features (optional, default: 0)
#'   }
#'   \item{\code{randomize}}{
#'     logical; randomize rows of data stream per simulation (optional, default: TRUE)
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
#'   \item{\code{new(offline_data, k, d = 0, arm_regression_models = NULL, single_regression_model = NULL,
#'   randomize = TRUE)}}{ generates and instantializes a new \code{OfflineDirectMethodBandit} instance. }
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
#' \code{\link{OfflineDirectMethodBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
NULL
