#' @export
LiSamplingOfflineBandit <- R6::R6Class(
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "LiSamplingOfflineBandit",
    initialize   = function(data_stream, k, d, d_disjoint = NULL, d_shared = NULL) {
      self$k <- k
      self$d <- d
      self$d_disjoint <- d_disjoint
      self$d_shared <- d_shared
      private$S <- data_stream$get_data_table()  # TODO: make this more general
    },
    get_context = function(index) {

      contextlist <- list(
        k = self$k,
        d = self$d,
        d_disjoint = self$d_disjoint,
        d_shared = self$d_shared,
        X = matrix(private$S$context[[index]], self$d, self$k)
      )
      contextlist
    },
    get_reward = function(index, context, action) {
      reward_at_index  <- as.double(private$S$reward[[index]])
      optimal_at_index <- as.double(private$S$optimal_reward_value[[index]])
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = reward_at_index,
          if (!is.null(optimal_at_index)) optimal_reward_value <- optimal_at_index
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Li Sampling Offline Evaluation
#'
#' \code{LiSamplingOfflineBandit} uses data from a randomly assigned policy for offline evaluation.
#'
#' The key assumption of the method is that the individual events are i.i.d., and
#' that the logging policy chose each arm at each time step uniformly at random.
#' Take care: if A is a stationary policy that does not change over trials,
#' data may be used more efficiently via propensity
#' scoring (Langford et al., 2008; Strehl et al., 2011) and related
#' techniques like doubly robust estimation (Dudik et al., 2011).
#'
#' @name LiSamplingOfflineBandit
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#'    bandit <- LiSamplingOfflineBandit(data_stream, k, d)
#' }
#'
#' @references
#'
#' Li, L., Chu, W., Langford, J., & Wang, X. (2011, February). Unbiased offline evaluation of contextual-bandit-based news article recommendation algorithms. In Proceedings of the fourth ACM international conference on Web search and data mining (pp. 297-306). ACM.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' @examples
#'
#' horizon            <- 100L
#'
NULL
