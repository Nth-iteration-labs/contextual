#' @export
BTSLogitPolicy <- R6::R6Class(
  "BTSLogitPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    initialize = function(alpha = 1.0, name = "BTSLogit") {
      super$initialize(name)
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms <- list( 'A' = diag(1, self$d, self$d), 'b' = rep(0,self$d), n = '')
    },
    get_action = function(context, t) {
      expected_rewards <- rep(0.0, context$k)
      X <- context$X
      for (arm in 1:context$k) {
        A          <-  theta$A[[arm]]
        b          <-  theta$b[[arm]]

        A.inv      <-  solve(A)
        theta.hat  <-  A.inv %*% b
        mean       <-  X %*% theta.hat

        variance              <-  sqrt(tcrossprod(context$X %*% A.inv, X))
        expected_rewards[arm] <- mean + (alpha * variance)
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      X <- context$X

      inc(theta$A[[arm]]) <- outer(X, X)
      inc(theta$b[[arm]]) <- reward * X

      theta
    }
  )
)


#' Policy: BTSLogitPolicy
#'
#' Each time step t, \code{BTS} runs a linear regression that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name BTSLogitPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- BTSLogitPolicy(alpha = 1.0, name = "LinUCB")
#' }
NULL
