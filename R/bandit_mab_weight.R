 #' @export
MabWeightBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    R = NULL,
    X = NULL,
    W = NULL
  ),
  public = list(
    class_name = "MabWeightBandit",
    initialize   = function(weights = NULL) {
      if (!is.null(weights)) self$set_weights(weights)
      private$X <- array(1, dim = c(self$d, self$k, 1))
    },
    get_weights = function() {
      private$W
    },
    set_weights = function(local_W) {
      if (is.vector(local_W)) private$W <- matrix(local_W, nrow = 1L)
      self$d <- as.integer(dim(private$W)[1])
      self$k <- as.integer(dim(private$W)[2])
      invisible(private$W)
    },
    get_context = function(t) {
      context <- list(
        k = self$k,
        d = self$d
      )
      context
    },
    get_reward = function(t, context, action) {
      private$R  <- as.double(matrix(runif(self$k) < self$get_weights(), self$k, self$d))
      reward <- list(
        reward                   = private$R[action$choice],
        optimal_reward_value     = private$R[which.max(private$R)]
      )
      reward
    }
  )
)

#' Bandit: MabWeightBandit
#'
#' MabWeightBandit intro
#'
#' @name MabWeightBandit
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{b <- MabWeightBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{MabWeightBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new MabWeightBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
NULL
