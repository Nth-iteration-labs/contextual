#' @export
SimpleBTSPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    J = NULL,
    a = NULL,
    b = NULL,
    class_name = "SimpleBTSPolicy",
    initialize = function(J = 100,
                          a = 1,
                          b = 1) {
      super$initialize()
      self$J  <- J
      self$a  <- a
      self$b  <- b
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('alpha' = rep(self$a,self$J),
                                 'beta'  = rep(self$b,self$J))
    },
    get_action = function(t, context) {
      point_estimate_of_mean <- vector("double", context$k)
      for (arm in 1:context$k) {
        one_replicate <- sample(self$J, 1)
        r_alpha <- self$theta$alpha[[arm]][one_replicate]
        r_beta  <- self$theta$beta[[arm]][one_replicate]
        point_estimate_of_mean[arm] <- r_alpha / (r_alpha + r_beta)
      }
      action$choice <- which_max_tied(point_estimate_of_mean)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      # double_or_nothing_bootstrap
      some_replicates = NULL
      while(length(some_replicates)==0)
        some_replicates <- which(rbinom(self$J, 1, .5) == 1)
      inc(self$theta$alpha[[arm]][some_replicates]) <- reward
      inc(self$theta$beta[[arm]][some_replicates])  <- 1 - reward
      self$theta
    }
  )
)


#' Policy: Thompson sampling with the online bootstrap
#'
#' Bootstrap Thompson Sampling
#'
#' Bootstrap Thompson Sampling (BTS) is a heuristic method
#' for solving bandit problems which modifies Thompson Sampling
#' (see \link{ThompsonSamplingPolicy}) by replacing the posterior distribution
#' used in Thompson sampling by a bootstrap distribution.
#'
#' @name SimpleBTSPolicy
#'
#' @section Usage:
#' \preformatted{
#' policy <- SimpleBTSPolicy(J = 100, a= 1, b = 1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{new(J = 100, a= 1, b = 1)}}{ Generates a new \code{SimpleBTSPolicy} object.
#'   Arguments are defined in the Argument section above.}
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
#' Eckles, D., & Kaptein, M. (2014). Thompson sampling with the online bootstrap.
#' arXiv preprint arXiv:1410.4009.
#'
#' Thompson, W. R. (1933). On the likelihood that one unknown probability exceeds another in
#' view of the evidence of two samples. Biometrika, 25(3/4), 285-294.
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
#' policy <- SimpleBTSPolicy(1000)
#' }
NULL
