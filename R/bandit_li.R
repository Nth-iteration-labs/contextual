#' @export
LiLogBandit <- R6::R6Class(
  "LiLogBandit",
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
      if (private$S$choice[[index]] == action$choice) {
        setNames(
          list(
            as.double(private$S$reward[[index]]),
            action$choice,
            as.integer(private$S$is_optimal[[index]]),
            as.double(private$S$oracle[[index]]),
            action$propensity
          ),
          c("reward", "choice", "is_optimal", "oracle", "propensity")
        )
      } else {
        NULL
      }
    }
  )
)

#' External LiLogBandit
#'
#' LiLogBandit intro
#'
#' @section Usage:
#' \preformatted{b <- LiLogBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{LiLogBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new LiLogBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name LiLogBandit
#' @examples
#'\dontrun{}
#'
NULL
