#' @export
LinUCBPolicy <- R6::R6Class(
  "LinUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = NULL,
    initialize = function(alpha = 1, name = "LinUCB") {
      super$initialize(name)
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$parameters <- list(
        'A' = diag(1,self$d),                                                   # A is a d*d identity matrix
        'b' = rep(0,self$d)                                                     # b is a 0 vector of length
      )
    },
    get_action = function(context) {
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        A          <- self$theta[[arm]]$A
        b          <- self$theta[[arm]]$b
        A.inv      <- chol2inv(chol(A))                                         # Faster than A.inv <- solve(A), same?
        theta.hat  <- A.inv %*% b
        mean       <-  context$X %*% theta.hat
        var        <-  sqrt(tcrossprod(context$X %*% A.inv, context$X))         # faster than sqrt( (context$X %*% A.inv ) %*% t(context$X) )
        expected_rewards[arm] <- mean + (self$alpha * var)
      }
      self$action$choice  <- self$argmax(expected_rewards)
      self$action$optimal_choice <- self$argmax(context$O)
      self$action
    },
    set_reward = function(reward, context) {
      X <- as.vector(context$X)
      self$theta[[reward$choice]]$A <-
        self$theta[[reward$choice]]$A + outer(X, X)

      self$theta[[reward$choice]]$b <-
        self$theta[[reward$choice]]$b + reward$reward * X

      self$theta
    }
  )
)


























#' External LinUCBPolicy
#'
#' LinUCBPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- LinUCBPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{LinUCBPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new LinUCBPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name LinUCBPolicy
#' @examples
#'\dontrun{}
#'
NULL
