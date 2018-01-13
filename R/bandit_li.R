# create from scratch if not cache, otherwise do cache
#' @export
LiLogBandit <- R6::R6Class(
  "LiLogBandit",
  inherit = BasicBandit,
  portable = FALSE,
  class = FALSE,
  private = list(
    .S = NULL # S: stream S of events of length L.
 ),
  public = list(
    initialize   = function(log_file, k, d) {
      super$initialize()
      self$k = k
      self$d = d
      private$.S = log_file$get_data_table()
    },
    get_context = function(t) {
      private$.X = matrix(private$.S$context[[t]], 1, self$d)
      self$context_to_list()
    },
    get_reward = function(action, t) {
      if (private$.S$choice[[t]] == action$choice) {
        return(
          setNames(
            list(
              as.integer(private$.S$reward[[t]]),
              action$choice,
              0,
              as.integer(private$.R[action$optimal_choice, t]),
              action$propensity
            ),
            c("reward",
              "choice",
              "is_optimal",
              "oracle",
              "propensity")
          )
        )
      } else {
        return(NULL)
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
