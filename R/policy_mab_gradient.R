#' @export
GradientPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    baseline = NULL,
    class_name = "GradientPolicy",

    initialize = function(alpha = 0.1, baseline = TRUE) {
      super$initialize()
      self$alpha <- alpha
      self$baseline <- baseline
    },
    set_parameters = function(context_params) {
      k <- context_params$k
      self$theta <- list('n' = 0, 'mean' = 0, 'pref' = rep(0,k), 'prob' = rep(0,k))
    },
    get_action = function(t, context) {
      exp_est <- exp(self$theta$pref)
      self$theta$prob <- exp_est/sum(exp_est)
      action$choice   <- sample(context$k, 1, prob = self$theta$prob)
      return(action)
    },
    set_reward = function(t, context, action, reward) {
      arm                  <- action$choice
      reward               <- reward$reward

      inc(self$theta$n)    <- 1
      inc(self$theta$mean) <- (reward - self$theta$mean) / self$theta$n

      one_hot              <- rep(0,context$k)
      one_hot[arm]         <- 1

      baseline             <- ifelse(self$baseline,self$theta$mean,0)

      self$theta$pref      <- self$theta$pref + self$alpha * (reward - baseline) * (one_hot-self$theta$prob)

      self$theta
    }
  )
)

#' Policy: Gradient
#'
#' \code{GradientPolicy} is a SoftMax type algorithm, based on Sutton & Barton (2018).
#'
#' @name GradientPolicy
#'
#' @section Usage:
#' \preformatted{
#' policy <- GradientPolicy(alpha = 0.1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{alpha = 0.1}}{
#'   double, temperature parameter alpha specifies how many arms we can explore.
#'   When alpha is high, all arms are explored equally, when alpha is low, arms offering higher
#'   rewards will be chosen.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(epsilon = 0.1)}}{ Generates a new \code{GradientPolicy} object. Arguments are defined in
#'   the Argument section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters()}}{each policy needs to assign the parameters it wants to keep track of
#'   to list \code{self$theta_to_arms} that has to be defined in \code{set_parameters()}'s body.
#'   The parameters defined here can later be accessed by arm index in the following way:
#'   \code{theta[[index_of_arm]]$parameter_name}
#'   }
#' }
#'
#' \describe{
#'   \item{\code{get_action(context)}}{
#'     here, a policy decides which arm to choose, based on the current values
#'     of its parameters and, potentially, the current context.
#'    }
#'   }
#'
#'  \describe{
#'   \item{\code{set_reward(reward, context)}}{
#'     in \code{set_reward(reward, context)}, a policy updates its parameter values
#'     based on the reward received, and, potentially, the current context.
#'    }
#'   }
#'
#' @references
#'
#' Kuleshov, V., & Precup, D. (2014). Algorithms for multi-armed bandit problems.
#' arXiv preprint arXiv:1402.6028.
#'
#' Cesa-Bianchi, N., Gentile, C., Lugosi, G., & Neu, G. (2017). Boltzmann exploration done right.
#' In Advances in Neural Information Processing Systems (pp. 6284-6293).
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
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights            <- c(0.9, 0.1, 0.1)
#'
#' policy             <- GradientPolicy$new(alpha = 0.1)
#' bandit             <- BasicBernoulliBandit$new(weights = weights)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#' plot(history, type = "arms")
NULL
