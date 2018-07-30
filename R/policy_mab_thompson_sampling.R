#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = 1,
    beta = 1,
    class_name = "ThompsonSamplingPolicy",
    initialize = function(alpha = 1, beta =  1) {
      super$initialize()
      self$alpha  <- alpha
      self$beta   <- beta
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('n' = 0, 'p' = 0.0, 'succes' = 0, 'mean' = 0.0)
    },
    get_action = function(t, context) {
      for (arm in 1:context$k) {
        self$theta$mean[arm] <- rbeta(
          1, alpha + self$theta$succes[[arm]], beta + self$theta$n[[arm]] - self$theta$succes[[arm]]
        )
      }
      action$choice <- max_in(self$theta$mean)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward

      inc(self$theta$n[[arm]]) <- 1
      if (reward == 1) inc(self$theta$succes[[arm]]) <- 1
      inc(self$theta$p[[arm]]) <- (reward - self$theta$p[[arm]]) / self$theta$n[[arm]]

      self$theta
    }
  )
)

#' Policy: Thompson Sampling
#'
#' \code{ThompsonSamplingPolicy} works by maintaining a prior on the the mean rewards of its arms.
#' In this, it follows a beta–binomial model with parameters alpha and beta, sampling values
#' for each arm from its prior and picking the arm with the highest value.
#' When an arm is pulled and a Bernoulli reward is observed, it modifies the prior based on the reward.
#' This procedure is repeated for the next arm pull.
#'
#' @name ThompsonSamplingPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- ThompsonSamplingPolicy(alpha = 1, beta = 1, name = "TSampling")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{alpha}}{
#'    integer, a natural number N>0 - first parameter of the Beta distribution
#'   }
#'   \item{\code{beta}}{
#'    integer, a natural number N>0 - second parameter of the Beta distribution
#'   }
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(alpha = 1, beta = 1)}}{ Generates a new \code{ThompsonSamplingPolicy} object. Arguments are defined in the Argument section above.}
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
#' Thompson, W. R. (1933). On the likelihood that one unknown probability exceeds another in view of the evidence of two samples. Biometrika, 25(3/4), 285-294.
#'
#' Chapelle, O., & Li, L. (2011). An empirical evaluation of thompson sampling. In Advances in neural information processing systems (pp. 2249-2257).
#'
#' Agrawal, S., & Goyal, N. (2013, February). Thompson sampling for contextual bandits with linear payoffs. In International Conference on Machine Learning (pp. 127-135).b
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}

#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights        <- c(0.9, 0.1, 0.1)
#'
#' policy             <- ThompsonSamplingPolicy$new(alpha = 1, beta = 1)
#' bandit             <- SyntheticBandit$new(weights = weights, precaching = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#'
NULL
