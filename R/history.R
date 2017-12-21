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
    initialize = function(n = 1) {
      self$n <- n

      self$data <- data.table::data.table(
        reward          = rep(0L,      n), #1
        optimal         = rep(0L,      n), #2
        context         = rep(list(),  n), #3
        agent           = rep("",      n), #4
        t               = rep(0L,      n), #5
        sim             = rep(0L,      n), #6
        arm             = rep(0L,      n), #7
        theta           = rep(list(),  n)  #8
      )
      invisible(self)
    },

    save_agent = function(counter, t, action, reward, context, policy_name, s = NA, theta = list()) {
      counter = as.integer(counter)
      if (reward$optimal) optimal = 1L else optimal = 0L

      # cant use I? Inhibit Interpretation/Conversion? maybe better lists in
      # diffent data.table?
      # add probability, propensity

      data.table::set(data, counter, 1L:8L,
                      c(reward[[1]],
                      optimal,
                      list(list(context)),
                      policy_name,
                      t,
                      s,
                      action$choice,
                      list(list(theta))
                      ))

    },
    save_data = function(filename = NA) {
      if (is.na(filename)) filename = paste("contextual_data_",
                         format(Sys.time(), "%y%m%d_%H%M%S"),
                         ".RData",
                         sep = "")
      saveRDS(self$data, file = filename, compress = FALSE)
      invisible(self)
    },
    load_data = function(filename) {
      self$data = readRDS(filename)
      invisible(self)
    },
    get_data_frame = function() {
      as.data.frame(self$data)
    },
    set_data_frame = function(dt) {
      self$data = as.data.table(dt)
      invisible(self)
    },
    get_data_table = function() {
      self$data
    },
    set_data_table = function(dt) {
      self$data = dt
      invisible(self)
    },
    delete_empty_rows = function() {
      self$data <- self$data[ sim > 0 & t > 0]
      self$data <- self$data[, t := seq_len(.N), by = c("agent","sim")]
      #self$data[ , max(t), by = c("agent","sim")][,min(V1), by = c("agent")][,V1]
      invisible(self)
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





