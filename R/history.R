#' @import data.table
#' @export
History <- R6::R6Class(
  "History",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    n = 1000,
    data = data.table::data.table(),
    initialize = function() {
    },
    reset = function(n = 1000) {
      self$n = n
      self$data = data.table::data.table(
        reward          = rep(0L,     n), #1
        optimal         = rep(0L,     n), #2
        agent           = rep("",     n), #3
        t               = rep(0L,     n), #4
        sim             = rep(0L,     n), #5
        arm             = rep(0L,     n), #6
        propensity      = rep(0.0,    n)  #7
      )
    },
    save_step = function(counter, t, s, action, reward, policy.name) {

      data.table::set(self$data, counter, 4L, t)
      data.table::set(self$data, counter, 5L, s)
      data.table::set(self$data, counter, 6L, action$current_choice)
      data.table::set(self$data, counter, 1L, reward$reward)
      data.table::set(self$data, counter, 3L, policy.name)
      if (is.null(action[["propensity"]]) ) {
        data.table::set(self$data, counter, 7L, NA)
      } else {
        data.table::set(self$data, counter, 7L, action$propensity)
      }

      if (reward$is_optimal_choice) {
        data.table::set(self$data, counter, 2L, 1L)
      } else {
        data.table::set(self$data, counter, 2L, 0L)
      }
    },
    get_data_table = function() {
      return(self$data)
    },
    set_data_table = function(dt) {
      self$data = dt
    }
  )
)

#' External History
#'
#' History intro
#'
#' @section Usage:
#' \preformatted{b <- History$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{History} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new History, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name History
#' @examples
#'\dontrun{}
#'
NULL
