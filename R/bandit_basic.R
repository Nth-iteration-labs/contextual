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
    initialize   = function(data = NULL) {
      super$initialize()
      if (!is.null(data)) set_weights(data)
    },
    get_weights = function() {
      private$W
    },
    set_weights = function(local_W) {
      if (is.vector(local_W)) private$W <- matrix(local_W, nrow = 1L)
      if (is.matrix(local_W)) private$W <- local_W
      self$d <- as.integer(dim(private$W)[1])
      self$k <- as.integer(dim(private$W)[2])
      private$O  <- t(private$W)
      invisible(private$W)
    },
    get_context = function(t) {
      private$context_to_list()
    },
    get_reward = function(action, t) {
      private$R <- matrix(runif(self$k) < self$get_weights(), self$k, self$d)
      private$reward_to_list(action)
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
