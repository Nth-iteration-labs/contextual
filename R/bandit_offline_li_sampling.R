#' @export
LiSamplingOfflineBandit <- R6::R6Class(
  "LiSamplingOfflineBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    initialize   = function(data_stream, k, d) {
      self$k <- k
      self$d <- d
      private$S <- data_stream$get_data_table()
    },
    get_context = function(index) {
      private$X <- array(matrix(private$S$context[[index]], self$d, self$k), dim = c(self$d, self$k, 1))
      contextlist <- list(
        k = self$k,
        d = self$d,
        X = private$X[,,1]
      )
      contextlist
    },
    do_action = function(context, action, index) {
      reward_at_index <- as.double(private$S$reward[[index]])
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = reward_at_index,
          optimal_reward_value = as.double(private$S$optimal_reward_value[[index]])
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
#' @family contextual bandits
#'
#' @section Usage:
#' \preformatted{
#' bandit <- LiSamplingOfflineBandit(data_stream, k, d)
#' }
#'
#'
#'
#' @references
#'
#' Li, L., Chu, W., Langford, J., & Wang, X. (2011, February). Unbiased offline evaluation of contextual-bandit-based news article recommendation algorithms. In Proceedings of the fourth ACM international conference on Web search and data mining (pp. 297-306). ACM.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}, \code{\link{Policy}}
#'
#' @examples
#'
#' horizon            <- 100L
#'
NULL
