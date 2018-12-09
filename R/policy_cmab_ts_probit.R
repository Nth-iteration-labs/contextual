#' @export
ContextualTSProbitPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "ContextualTSProbitPolicy",
    means = NULL,
    sigmas = NULL,
    draws = NULL,
    use_prop = NULL,
    initialize = function(means = 0, sigmas = 1, draws = 1000, use_prop = FALSE) {
      self$means = means
      self$sigmas = sigmas
      self$draws = draws
      self$use_prop = use_prop
    },
    set_parameters = function(context_params) {
      self$theta         <- list( "main_mu"    = rep(self$means,context_params$d),
                                  "main_sigma" = rep(self$sigmas,context_params$d))
      self$theta_to_arms <- list( "arm_mu"    =  rep(self$means,context_params$d),
                                  "arm_sigma" =  rep(self$sigmas,context_params$d))
    },
    get_action = function(t, context) {

      J <- self$draws
      pred <- matrix(NA, nrow=context$k, ncol=J)

      for (arm in 1:context$k) {
        # main plus interaction
        mus    <- c(self$theta$main_mu,self$theta$arm_mu[[arm]])
        sigmas <- c(self$theta$main_sigma,self$theta$arm_sigma[[arm]])

        Xa     <- get_arm_context(context$X, arm)

        # Sample J times from a normal mean and sd=se (exploration)
        betas <- matrix(rnorm(2*context$d*J, mus, sigmas), ncol=J, nrow=2*context$d)
        pred[arm,] <- t(c(Xa,Xa))%*%betas

      }
      wins          <- apply(t(pred),1,which.max)
      action$choice <- sample(wins,1)

      # propensity score
      tab <- table(factor(wins, level=c(1:context$k)))
      action$propensity <- as.numeric((tab/sum(tab))[action$choice])

      action
    },
    set_reward = function(t, context, action, reward) {
      arm                            <- action$choice
      reward                         <- reward$reward

      Xa                             <- get_arm_context(context$X, arm)
      y                              <- reward*2-1 # -1,1

      if(self$use_prop)              y <- y * 1/action$propensity

      mus                            <- c(self$theta$main_mu,self$theta$arm_mu[[arm]])
      sigmas                         <- c(self$theta$main_sigma,self$theta$arm_sigma[[arm]])

      bopr_result                    <- bopr(c(Xa,Xa), y, mus, sigmas)

      self$theta$main_mu             <- bopr_result[1,1:context$d]
      self$theta$arm_mu[[arm]]       <- bopr_result[1,(context$d+1):(context$d*2)]

      self$theta$main_sigma          <- bopr_result[2,1:context$d]
      self$theta$arm_sigma[[arm]]    <- bopr_result[2,(context$d+1):(context$d*2)]
      self$theta
    },
    bopr = function(x,y,mu=rep(0,length(x)),sigma2=rep(10,length(x)),beta=.05){
      total_mean <- sum(mu)
      total_variance <- sum(sigma2)
      t <- y * total_mean / sqrt(total_variance)
      t <- bopr_clip(t,-5,5)
      v <- dnorm(t) / pnorm(t)
      w <- v * (v + t)
      for(j in c(1:length(x))){
        if(x[j]==0) next

        mean_delta <- y * sigma2[j] / sqrt(total_variance) * v
        variance_multiplier <- 1.0 - sigma2[j] / total_variance * w

        mu[j] <- mu[j] + mean_delta
        sigma2[j] <- sigma2[j] * variance_multiplier
      }
      rbind(mu,sigma2)
    },
    bopr_clip = function(x, a, b) {
      a + (x - a > 0) * (x - a) - (x - b > 0) * (x - b)
    }
  )
)

#' Policy: ContextualTSProbitPolicy
#'
#' @name ContextualTSProbitPolicy
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualTSProbitPolicy()
#' }
NULL
