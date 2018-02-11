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
      self$parameters <- list( 'A' = diag(1,self$d), 'b' = rep(0,self$d))
    },
    get_action = function(context) {
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        A          <-  theta[[arm]]$A
        b          <-  theta[[arm]]$b
        A.inv      <-  chol2inv(chol(A))
        theta.hat  <-  A.inv %*% b
        mean       <-  context$X %*% theta.hat
        var        <-  sqrt(tcrossprod(context$X %*% A.inv, context$X))
        expected_rewards[arm] <- mean + (alpha * var)
      }
      action$choice  <- argmax(expected_rewards)
      action
    },
    set_reward = function(reward, context) {
      arm <- reward$choice
      reward <- reward$reward
      X <- as.vector(context$X)
      inc(theta[[arm]]$A) <- outer(X, X)
      inc(theta[[arm]]$b) <- reward * X
      theta
    }
  )
)
























# A is a d*d identity matrix, b is a 0 vector of length

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
