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
    .is_precaching = FALSE
  ),
  active = list(
    is_precaching = function(value) {
      if (missing(value)) {
        private$.is_precaching
      } else {
        warning("### Precaching is locked to FALSE by default.",
                call. = FALSE)
      }
    }
  ),
  public = list(
    d            = NULL,
    k            = NULL,
    initialize   = function() {
      super$initialize()
      private$.X <- matrix(1L, 1L, 1L)
      private$.R <- matrix(0L, 3L, 1L)
      private$.W <- matrix(0L, 3L, 1L)
      private$.O <- matrix(0L, 3L, 1L)
    },
    get_context = function(t = 1) {
      stop("### Need to implement get_context.",
           call. = FALSE)
    },
    get_reward = function(action, t = 1) {
      stop("### Need to implement get_reward.",
           call. = FALSE)
    },
    set_weights = function(W) {
      stop("### Need to implement set_weights.",
           call. = FALSE)
    },
    generate_cache = function(n = 1) {
      stop("### Need to implement generate_cache if is_precaching is TRUE.",
           call. = FALSE)
    },
    context_to_list = function(t = 1) {
      return(
        setNames(list(self$k, self$d, private$.X[t, ], private$.O[, t]),
                 c("k", "d", "X", "O")))
    },
    reward_to_list = function(action, t = 1) {
      setNames(
        list(
          as.integer(private$.R[action$choice, t]),
          action$choice,
          argmax(private$.R[, t]) == action$choice,
          as.integer(private$.R[action$optimal_choice, t]),
          action$propensity
        ),
        c("reward",
          "choice",
          "is_optimal",
          "oracle",
          "propensity")
      )
    },
    object_size = function() {
      cat(paste("  Bandit: ", self$hash),"\n")
      cat(paste("    Size of W:        ",
                format(object.size(private$.W), units = "auto")),"\n")
      cat(paste("    Size of R:        ",
                format(object.size(private$.R), units = "auto")),"\n")
      cat(paste("    Size of X:        ",
                format(object.size(private$.X), units = "auto")),"\n")
      cat(paste("    Size of O:        ",
                format(object.size(private$.O), units = "auto")),"\n")
      self$hash
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
