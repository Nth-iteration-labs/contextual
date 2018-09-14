#' @export
LiSamplingOfflineBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "LiSamplingOfflineBandit",
    randomize = NULL,
    initialize   = function(data_stream, k, d, unique = NULL, shared = NULL, randomize = TRUE) {
      self$k <- k               # Number of arms (integer)
      self$d <- d               # Dimension of context feature vector (integer)
      self$randomize <-randomize
      private$S <- data_stream  # Data stream, here as a data.table
    },
    post_initialization = function() {
      if(isTRUE(self$randomize))private$S <- private$S[sample(nrow(private$S))]
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

      reward           <- as.double(private$S$reward[[index]])
      optimal_reward   <- as.double(private$S$optimal_reward[[index]])
      optimal_arm      <- as.double(private$S$optimal_arm[[index]])

      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = reward,
          optimal_reward = optimal_reward,
          optimal_arm = optimal_arm
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
#'
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
