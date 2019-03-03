#' @export
ContextualLogitBTSPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    J = NULL,
    use_prop = NULL,
    class_name = "ContextualLogitBTSPolicy",
    initialize = function(draws = 1000, use_prop = FALSE) {
      self$J   <- draws
      self$use_prop <- use_prop
    },
    set_parameters = function(context_params) {
      # data, represents main + interactions
      self$theta         <- list( "M"        = matrix(0,self$J,context_params$d))
      self$theta_to_arms <- list( "i"        = matrix(0,self$J,context_params$d))
    },
    get_action = function(t, context) {
      pred          <- matrix(0.0, self$J, context$k)
      for (arm in 1:context$k) {
        # main plus interaction
        coefs <- cbind(self$theta$M,self$theta$i[[arm]])
        Xa    <- get_arm_context(context, arm)
        pred[,arm]  <- coefs%*%c(Xa,Xa)
      }
      wins          <- apply(pred,1,which_max_tied)
      action$choice <- sample(wins,1)

      # propensity score
      tab <- table(factor(wins, level=c(1:context$k)))
      action$propensity <- as.numeric((tab/sum(tab))[action$choice])

      action
    },
    set_reward = function(t, context, action, reward) {
      arm                       <- action$choice
      reward                    <- reward$reward

      if(self$use_prop)         reward <- reward * 1/action$propensity

      X                         <- get_arm_context(context, arm)

      update                    <- which(rbinom(self$J,1,.5)==1)

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
#' @name ContextualLogitBTSPolicy
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualLogitBTSPolicy()
#' }
NULL
