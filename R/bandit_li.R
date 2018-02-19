#' @export
OfflineLiBandit <- R6::R6Class(
  "OfflineLiBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    initialize   = function(data_file, k, d) {
      self$k <- k
      self$d <- d
      private$S <- data_file$get_data_table()
    },
    get_context = function(index) {
      private$X <- matrix(private$S$context[[index]], 1, self$d)
      private$context_to_list()
    },
    get_reward = function(action, index) {
      use_ips <- FALSE
      if (use_ips == TRUE) {
        reward_at_index <- as.double(private$S$reward[[index]])
        reward_at_index <- reward_at_index/as.double(private$S$propensity[[index]])

        #cost .. blabla

      } else {
        reward_at_index <- as.double(private$S$reward[[index]])
      }

      if (private$S$choice[[index]] == action$choice | use_ips == TRUE) {
        list(
          reward = reward_at_index,
          is_optimal = as.integer(private$S$is_optimal[[index]]),
          oracle = as.double(private$S$oracle[[index]])
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Li Offline Evaluation
#'
#' \code{OfflineLiBandit} uses data from a randomly assigned policy for offline evaluation.
#' The key assumption of the method is that the individual events are i.i.d., and
#' that the logging policy chose each arm at each time step uniformly at random.
#' Take care: if A is a stationary policy that does not change over trials,
#' data may be used more efficiently via propensity
#' scoring (Langford et al., 2008; Strehl et al., 2011) and related
#' techniques like doubly robust estimation (Dudik et al., 2011).
#'
#' @name OfflineLiBandit
#' @family contextual bandits
#'
#' @section Usage:
#' \preformatted{
#' bandit <- OfflineLiBandit(data_file, k, d)
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
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}, \code{\link{AbstractPolicy}}
#'
#' @examples
#'
#' horizon            <- 100L
#'
NULL
