#' @export
AbstractBandit <- R6::R6Class(
  "AbstractBandit",
  portable = TRUE,
  class = FALSE,
  private = list(

    W = NULL,      # weights matrix k*d
    R = NULL,      # rewards matrix
    X = NULL,      # context matrix
    O = NULL,      # oracle matrix

    .hash = NULL,

    precaching = FALSE,

    # utility functions

    context_to_list = function(t) {
      if (self$is_precaching) idx <- t else idx <- 1
      setNames(list(self$k, self$d, private$X[idx, ], private$O[, idx]),
               c("k", "d", "X", "O"))
    },
    max_in = function(x, equal_is_random = TRUE)
    {
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L)  {
        if (equal_is_random) {
          return(sample(y, 1L, replace = TRUE))
        } else {
          return(y[1])
        }
      } else {
        return(y)
      }
    },
    reward_to_list = function(action, t) {
      if (self$is_precaching) idx <- t else idx <- 1
      setNames(
        list(
          private$R[action$choice, idx],
          action$choice,
          private$max_in(private$R[, idx]) == action$choice,
          as.double(private$R[private$max_in(private$O[, idx]), idx]),
          action$propensity
        ),
        c("reward", "choice", "is_optimal", "oracle", "propensity")
      )
    }
  ),
  active = list(
    is_precaching = function(value) {
      if (missing(value)) {
        private$precaching
      } else {
        warning("## Precaching is locked to FALSE by default.",
                call. = FALSE)
      }
    },
    hash = function(value) {
      if (missing(value)) {
        private$.hash
      }
    }
  ),
  public = list(
    d            = NULL,
    k            = NULL,
    initialize   = function() {
      private$.hash = sub('<environment: (.*)>', '\\1',  capture.output(self))
      private$X <- matrix(1L, 1L, 1L)
      private$R <- matrix(0L, 3L, 1L)
      private$W <- matrix(0L, 3L, 1L)
      private$O <- matrix(0L, 3L, 1L)
    },
    set_seed = function(seed) {
      set.seed(seed)
    },
    get_context = function(t) {
      stop("You still need to implement AbstractBandit$get_context.",
           call. = FALSE)
    },
    get_reward = function(action, t) {
      stop("You still need to implement AbstractBandit$get_reward.",
           call. = FALSE)
    },
    set_weights = function(W) {
      stop("You still need to implement AbstractBandit$set_weights.",
           call. = FALSE)
    },
    object_size = function() {
      cat(paste("  Bandit: ", self$hash),"\n")
      cat(paste("    Size of W:        ", format(object.size(private$W), units = "auto")),"\n")
      cat(paste("    Size of R:        ", format(object.size(private$R), units = "auto")),"\n")
      cat(paste("    Size of X:        ", format(object.size(private$X), units = "auto")),"\n")
      cat(paste("    Size of O:        ", format(object.size(private$O), units = "auto")),"\n")
      self$hash
    },
    generate_bandit_data = function(n) {
      stop("You still need to implement AbstractBandit$generate_cache if is_precaching is TRUE.",
           call. = FALSE)
    }
  )
)

#' External AbstractBandit
#'
#' AbstractBandit intro
#'
#' @section Usage:
#' \preformatted{b <- AbstractBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{AbstractBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new AbstractBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @family contextual classes
#' @name AbstractBandit
#' @examples
#'\dontrun{}
#'
NULL
