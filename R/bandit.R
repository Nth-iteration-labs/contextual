# create from scratch if not cache, otherwise do cache
#' @export
AbstractBandit <- R6::R6Class(
  "AbstractBandit",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  private = list(

    .W = NULL,      # weights k*d
    .R = NULL,      # rewards matrix
    .X = NULL,      # context matrix
    .O = NULL,      # oracle

    .is_precaching = NULL,

    .format_reward = function(action, t = 1) {
      setNames(
        list(
          as.integer(private$.R[action$choice, t]),
          action$choice,
          argmax(private$.R[, t]) == action$choice,
          action$propensity
        ),
        c("reward",
          "choice",
          "is_optimal_choice",
          "propensity")
      )
    },
    .format_context = function(t = 1) {
      return(
        setNames(list(self$k, self$d, private$.X[t, ], private$.O[, t]),
                 c("k", "d", "X", "oracle")))
    }
  ),
  active = list(
    is_precaching = function(value) {
      if (missing(value)) {
        private$.is_precaching
      } else {
        private$.is_precaching <- value
      }
    }
  ),
  public = list(
    d            = NULL,
    k            = NULL,
    initialize   = function() {
      self$is_precaching <- FALSE
      private$.X <- matrix(1, 1, 1)
      private$.R <- matrix(0, 3, 1)
      private$.W <- matrix(0, 3, 1)
      private$.O <- matrix(0, 3, 1)
    },
    get_weights = function() {
      private$.W
    },
    set_weights = function(W) {
      if (is.vector(W))
        private$.W <- matrix(W, nrow = 1)
      if (is.matrix(W))
        private$.W <- W
      self$d <- dim(private$.W)[1]
      self$k <- dim(private$.W)[2]
      private$.O  <- t(private$.W)
      invisible(self)
    },
    get_reward = function(action, t) {
      private$.format_reward(action)
    },
    get_context = function(t) {
      private$.format_context(1)
    },
    calculate_reward = function (R){
      private$.R <- matrix(R,3,1)
    },
    generate_cache = function(n) {
      stop("Need to implement cache if is_precaching is TRUE.")
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
#' @name AbstractBandit
#' @examples
#'\dontrun{}
#'
NULL
