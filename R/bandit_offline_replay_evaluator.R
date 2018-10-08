#' @export
OfflineReplayEvaluatorBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL,
    oa = NULL,
    or = NULL
  ),
  public = list(
    class_name = "OfflineReplayEvaluatorBandit",
    randomize = NULL,
    initialize   = function(offline_data, k, d, unique = NULL, shared = NULL, randomize = TRUE) {
      self$k <- k                 # Number of arms (integer)
      self$d <- d                 # Dimension of context feature vector (integer)
      self$randomize <-randomize  # Randomize logged events for every simulation? (logical)
      private$S <- offline_data   # Logged events (by default, as a data.table)

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
        X = matrix(private$S$context[[index]], self$d, self$k)
      )
      context
    },
    get_reward = function(index, context, action) {
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = as.double(private$S$reward[[index]]),

          optimal_reward = ifelse(private$or,
                                  as.double(private$S$optimal_reward[[index]]),
                                  NA),

          optimal_arm    = ifelse(private$oa,
                                  as.double(private$S$optimal_arm[[index]]),
                                  NA)
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Li's Offline Policy Evaluator
#'
#' Policy for the evaluation of policies with offline data.
#'
#' The key assumption of the method is that that the original logging policy chose
#' i.i.d. arms uniformly at random.
#'
#' Take care: if the original logging policy does not change over trials, data may be
#' used more efficiently via propensity scoring (Langford et al., 2008; Strehl et al., 2011)
#' and related techniques like doubly robust estimation (Dudik et al., 2011).
#'
#' @name OfflineReplayEvaluatorBandit
#'
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineReplayEvaluatorBandit(offline_data, k, d, unique = NULL, shared = NULL, randomize = TRUE)
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
#'     integer; number of contextual features (required)
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
#'   \item{\code{new(offline_data, k, d, unique = NULL, shared = NULL, randomize = TRUE)}}{ generates
#'    and instantializes a new \code{OfflineReplayEvaluatorBandit} instance. }
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
#' Agrawal, R. (1995). The continuum-armed bandit problem. SIAM journal on control and optimization, 33(6), 1926-1951.
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
#' ## generate random policy log and save it
#'
#' context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
#'                                   0.1, 0.9, 0.1,
#'                                   0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
#' horizon     <- 2000L
#' simulations <- 1L
#' bandit      <- ContextualBernoulliBandit$new(weights = context_weights)
#'
#' # For the generation of random data choose a random policy,
#' # otherwise rejection sampling will produce biased results.
#'
#' policy      <- RandomPolicy$new()
#'
#' agent       <- Agent$new(policy, bandit)
#'
#' simulation  <-
#'   Simulator$new(
#'     agent,
#'     horizon = horizon,
#'     simulations = simulations,
#'     save_context = TRUE
#'   )
#'
#' random_offline_data <- simulation$run()
#' random_offline_data$save("log.RData")
#'
#' ## use saved log to evaluate policies with OfflineReplayEvaluatorBandit
#'
#' history <- History$new()
#' history$load("log.RData")
#' log_S <- history$get_data_table()
#'
#' bandit <- OfflineReplayEvaluatorBandit$new(offline_data = log_S, k = 3, d = 3)
#'
#' agents <-
#'   list(
#'     Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
#'     Agent$new(LinUCBDisjointPolicy$new(0.6), bandit)
#'   )
#'
#' simulation <-
#'   Simulator$new(
#'     agents,
#'     horizon = horizon,
#'     simulations = simulations,
#'     t_over_sims = TRUE,
#'     do_parallel = FALSE,
#'     reindex = TRUE
#'   )
#'
#' li_bandit_history <- simulation$run()
#'
#' plot(after, regret = FALSE, type = "cumulative", rate = TRUE)
#'
#' if (file.exists("log.RData")) file.remove("log.RData")
#'
#' }
NULL
