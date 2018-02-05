#' @import data.table
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  class = TRUE,
  public = list(
    n = 1000,
    save_theta = NULL,
    save_context = NULL,
    initialize = function(n = 1, save_context = FALSE, save_theta = FALSE) {
      self$n <- n
      self$save_context <- save_context
      self$save_theta   <- save_theta
      self$reset()
    },
    reset = function() {
      private$.data <- data.table::data.table(
        t               = rep(0L,      n),
        sim             = rep(0L,      n),
        choice          = rep(0L,      n),
        reward          = rep(0.0,     n),
        is_optimal      = rep(0L,      n),
        oracle          = rep(0.0,     n),
        agent           = rep("",      n)
      )
      if (self$save_context) private$.data$context <- rep(list(),  n)
      if (self$save_theta)   private$.data$theta   <- rep(list(),  n)
      invisible(self)
    },
    save = function(
                       index,
                       t,
                       action,
                       reward,
                       policy_name,
                       s,
                       context_value = NA,
                       theta_value = NA

                     ) {

      index <- as.integer(index)
      if (reward$is_optimal) is_optimal <- 1L else is_optimal <- 0L
      data.table::set(
                       data,
                       index, 1L:7L,
                       list(t,s,action$choice,reward$reward,
                            is_optimal,reward$oracle,policy_name)
                      )
      if (self$save_context & !self$save_theta) {
        data.table::set(data, index, 8L , list(list(context_value)))
      } else if (!self$save_context & self$save_theta) {
        data.table::set(data, index, 8L, list(list(theta_value)))
        ## if split over mult col
        #data[index, (paste0("X.", seq_along(context))) := context]
      } else if (self$save_context & self$save_theta) {
        data.table::set(data, index, 8L, list(list(context_value)))
        data.table::set(data, index, 9L , list(list(theta_value)))
      }
      invisible(self)
    },
    save_data = function(filename = NA) {
      if (is.na(filename))
        filename <- paste("contextual_data_",
                         format(Sys.time(), "%y%m%d_%H%M%S"),
                         ".RData",
                         sep = "")
      saveRDS(private$.data, file = filename, compress = FALSE)
      invisible(self)
    },
    load_data = function(filename) {
      private$.data <- readRDS(filename)
      invisible(self)
    },
    get_data_frame = function() {
      as.data.frame(private$.data)
    },
    set_data_frame = function(df) {
      private$.data <- as.data.table(df)
      invisible(self)
    },
    get_data_table = function() {
      private$.data
    },
    set_data_table = function(dt) {
      private$.data <- dt
      invisible(self)
    },
    delete_empty_rows = function() {
      private$.data <- private$.data[sim > 0 & t > 0]
      private$.data <- private$.data[, t := seq_len(.N), by = c("agent", "sim")]
      #private$.data[ , max(t), by = c("agent","sim")][,min(V1), by = c("agent")][,V1]
      invisible(self)
    }
  ),
  active = list(
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        warning("## History$data is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    .data = NULL
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
