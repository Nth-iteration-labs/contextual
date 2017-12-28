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
        t               = rep(0L,      n),
        sim             = rep(0L,      n),
        arm             = rep(0L,      n),
        reward          = rep(0L,      n),
        optimal         = rep(0L,      n),
        agent           = rep("",      n),
        context         = rep(list(),  n),
        theta           = rep(list(),  n)
      )
      invisible(self)
    },

    save_agent = function(counter,
                          t,
                          action,
                          reward,
                          policy_name,
                          s,
                          context = NA,
                          theta = NA) {

      counter = as.integer(counter)

      if (reward$optimal)
        optimal = 1L
      else
        optimal = 0L

      data.table::set(data, counter, 1L , t)
      data.table::set(data, counter, 2L , s)
      data.table::set(data, counter, 3L , action$choice)
      data.table::set(data, counter, 4L , reward[[1]])
      data.table::set(data, counter, 5L , optimal)
      data.table::set(data, counter, 6L , policy_name)
      if (!is.na(context)) {
        data.table::set(data, counter, 7L , list(list(context)))
      }
      if (!is.na(theta)) {
        data.table::set(data, counter, 8L , list(list(theta)))
      }
    },
    save_data = function(filename = NA) {
      if (is.na(filename))
        filename = paste("contextual_data_",
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
      self$data <- self$data[sim > 0 & t > 0]
      self$data <-
        self$data[, t := seq_len(.N), by = c("agent", "sim")]
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
