library(R6)
#' @export
LinUCBPolicy <- R6Class(
  "LinUCBPolicy",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    alpha = 0.1,
    name = "",
    action = list(),
    initialize = function(alpha = 1, name = "LinUCB") {
      self$alpha = alpha
      self$name = name
      self$action = list()
    },
    get_action = function(agent, context) {
      expected.rewards.vector = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        A          = agent$get_memory()[['theta']][[arm]][['A']]
        b          = agent$get_memory()[['theta']][[arm]][['b']]
        A.inv      = chol2inv(chol(A))                                          # Faster than A.inv = solve(A), same?
        theta.hat  = A.inv %*% b
        mean =  context$X %*% theta.hat
        var  =  sqrt(tcrossprod(context$X %*% A.inv, context$X))              # faster than sqrt( (context$X %*% A.inv ) %*% t(context$X) )
        expected.rewards.vector[arm] = mean + (self$alpha * var)
      }
      self$action$current_choice  = index_of_max(expected.rewards.vector)
      self$action$propensity      = 0 # ###################
      return(self$action)
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
