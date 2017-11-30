library(R6)
library(MASS)
#' @export
LinUCBPolicy <- R6Class(
  "LinUCBPolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    alpha = 0.1,
    name = "",
    initialize = function(alpha = 1, name = "LinUCB") {
      self$alpha = alpha
      self$name = name
    },
    get.action = function(agent,context) {
      expected.rewards.vector = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        A          = agent$get.memory()[['theta']][[arm]][['A']]
        b          = agent$get.memory()[['theta']][[arm]][['b']]
        A.inv      = chol2inv(chol(A))                                        # Faster as A.inv = solve(A), same?
        theta.hat  = A.inv %*% b
        mean =  context$X %*% theta.hat
        var  =  sqrt( tcrossprod(context$X %*% A.inv, context$X) )            # faster as sqrt( (context$X %*% A.inv ) %*% t(context$X) )
        expected.rewards.vector[arm] = mean + (self$alpha * var)
      }
      return(index.of.max(expected.rewards.vector))
    }
  )
)
