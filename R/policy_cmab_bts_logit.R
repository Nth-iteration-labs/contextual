#' @export
ContextualLogitBTSPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    J = NULL,
    class_name = "ContextualLogitBTSPolicy",
    initialize = function(J = 100) {
      self$J  <- J
    },
    set_parameters = function(context_params) {
      # data, represents main + interactions
      self$theta         <- list( "M"        = matrix(0,self$J,context_params$d))
      self$theta_to_arms <- list( "i"        = matrix(0,self$J,context_params$d))
    },
    get_action = function(t, context) {
      X             <- rbind(context$X,context$X)
      pred          <- matrix(0.0, self$J, context$k)
      for (arm in 1:context$k) {
        # main plus interaction
        coefs <- cbind(self$theta$M,self$theta$i[[arm]])
        pred[,arm]  <- coefs%*%X[, arm]
      }
      wins          <- apply(pred,1,which.max)
      action$choice <- sample(wins,1)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      X      <- c(context$X[, arm])

      update <- which(rbinom(self$J,1,.5)==1)
      # Loop through each J to be updated and update betas
      for (i in update) {
        # update each replicate using sgd
        self$theta$M[i,]        <- self$sgd(reward, X, self$theta$M[i,])
        self$theta$i[[arm]][i,] <- self$sgd(reward, X, self$theta$i[[arm]][i,])
      }
      self$theta
    },
    sgd = function(y, X, beta, lambda=.1){
      p <- self$inv_logit(X%*%beta)
      beta + lambda*(y-p)%*%X
    },
    inv_logit = function(x){
      exp(x)/(1+exp(x))
    }
  )
)

#' Policy: ContextualLogitBTSPolicy
#'
#' Each time step t, \code{ContextualLogitBTSPolicy} runs ...
#'
#' @name ContextualLogitBTSPolicy
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualLogitBTSPolicy()
#' }
NULL
