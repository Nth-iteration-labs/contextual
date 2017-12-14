#' @import data.table
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  public = list(
    n = 1000,
    data = data.table::data.table(),
    initialize = function() {

    },
    reset = function(n = 1000) {
      self$n <- n
      self$data <- data.table::data.table(
        reward          = rep(0L,      n), #1
        optimal         = rep(0L,      n), #2
        agent           = rep("",      n), #3
        t               = rep(0L,      n), #4
        sim             = rep(0L,      n), #5
        arm             = rep(0L,      n), #6
        theta           = rep(list(),  n) #7
        #propensity      = rep(0.0,    n)  #8

      )
    },
    save_agent = function(counter, t, action, reward, policy_name, s = NA, theta = list()) {
      counter = as.integer(counter)
      if (reward$is_optimal_choice) opt = 1L else opt = 0L
      #ugly but optimized
      data.table::set(data, counter, 1L:7L,
                      c(reward[[1]],
                      opt,
                      policy_name,
                      t,
                      s,
                      action$choice,
                      list(list(theta))
                      ))

    },
    get_data_table = function() {
      self$data
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





