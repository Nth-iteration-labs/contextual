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
    .is_precaching = NULL
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
      super$initialize()
      self$is_precaching <- FALSE
      private$.X <- matrix(1L, 1L, 1L)
      private$.R <- matrix(0L, 3L, 1L)
      private$.W <- matrix(0L, 3L, 1L)
      private$.O <- matrix(0L, 3L, 1L)
    },
    get_weights = function() {
      private$.W
    },
    object_size = function() {
      cat(paste("  Bandit: ", self$hash),"\n")
      cat(paste("    Size of W: ", format(object.size(private$.W), units = "auto")),"\n")
      cat(paste("    Size of R: ", format(object.size(private$.R), units = "auto")),"\n")
      cat(paste("    Size of X: ", format(object.size(private$.X), units = "auto")),"\n")
      cat(paste("    Size of O: ", format(object.size(private$.O), units = "auto")),"\n")
      self$hash
    },
    set_weights = function(W) {
      if (is.vector(W)) private$.W <- matrix(W, nrow = 1L)
      if (is.matrix(W)) private$.W <- W
      self$d <- as.integer(dim(private$.W)[1])
      self$k <- as.integer(dim(private$.W)[2])
      private$.O  <- t(private$.W)
      invisible(self)
    },
    get_context = function(t) {
      self$context_to_list()
    },
    get_reward = function(action, t) {
      self$reward_to_list(action)
    },
    calculate_reward = function(R){
      private$.R <- matrix(R,3L,1L)
    },
    generate_cache = function(n) {
      stop("Need to implement cache if is_precaching is TRUE.")
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
          action$propensity
        ),
        c("reward",
          "choice",
          "optimal",
          "propensity")
      )
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
