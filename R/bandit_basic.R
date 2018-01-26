# create from scratch if not cache, otherwise do cache
# This is not BasicBandit, but UniformBasicBandit..
# even stronger: this is a non-contextual bandit ... do something about this, or just clear up by name
#' @export
BasicBandit <- R6::R6Class(
  "BasicBandit",
  inherit = AbstractBandit,
  portable = FALSE,
  class = FALSE,
  public = list(
    initialize   = function() {
      super$initialize()
    },
    get_weights = function() {
      private$.W
    },
    set_weights = function(W) {
      if (is.vector(W)) private$.W <- matrix(W, nrow = 1L)
      if (is.matrix(W)) private$.W <- W
      self$d <- as.integer(dim(private$.W)[1])
      self$k <- as.integer(dim(private$.W)[2])
      private$.O  <- t(private$.W)
      invisible(private$.W)
    },
    get_context = function(t = 1) {
      self$context_to_list()
    },
    get_reward = function(action, t = 1) {
      private$.R <- matrix(runif(self$k) < self$get_weights(), self$k, self$d)
      self$reward_to_list(action)
    }
  )
)

#' External BasicBandit
#'
#' BasicBandit intro
#'
#' @section Usage:
#' \preformatted{b <- BasicBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{BasicBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new BasicBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name BasicBandit
#' @examples
#'\dontrun{}
#'
NULL
