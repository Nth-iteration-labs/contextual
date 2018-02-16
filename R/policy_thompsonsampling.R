#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = 1,
    beta = 1,
    initialize = function(alpha = 1, beta =  1, name = "Thompson Sampling") {
      super$initialize(name)
      self$alpha  <- alpha
      self$beta   <- beta
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'p' = 0.0, 'succes' = 0, 'mean' = 0.0)
    },
    get_action = function(context) {
      for (arm in 1:context$k) {
        theta$mean[arm] <-  rbeta(
          1, alpha + theta$succes[[arm]], beta + theta$n[[arm]] - theta$succes[[arm]]
        )
      }
      action$choice <- max_in(theta$mean)
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward

      inc(theta$n[[arm]]) <- 1
      if (reward == 1) inc(theta$succes[[arm]]) <- 1
      inc(theta$p[[arm]]) <- (reward - theta$p[[arm]]) / theta$n[[arm]]

      theta
    }
  )
)

#' Policy: Thompson Sampling
#'
#' \code{ThompsonSamplingPolicy} ...
#'
#' @name ThompsonSamplingPolicy
#' @family contextual classes
#'
#' @section Usage:
#' \preformatted{
#' policy <- ThompsonSamplingPolicy(alpha = 1, beta = 1, name = "TSampling")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(name = "Random")}}{ Generates a new \code{ThompsonSamplingPolicy} object. Arguments are defined in the Argument section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters()}}{each policy needs to assign the parameters it wants to keep track of
#'   to list \code{self$parameters} that has to be defined in \code{set_parameters()}'s body.
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
#' Auer, P., Cesa-Bianchi, N., Freund, Y., & Schapire, R. E. (2002). The nonstochastic multiarmed bandit problem. SIAM journal on computing, 32(1), 48-77. Strehl, A., & Littman, M. (2004). Exploration via modelbased interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' @seealso
#'
#' Online: \href{https://nth-iteration-labs.github.io/contextual/index.html}{Documentation}
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weight_per_arm     <- c(0.9, 0.1, 0.1)
#'
#' policy             <- ThompsonSamplingPolicy$new(alpha = 1, beta = 1, name = "TSampling")
#' bandit             <- SyntheticBandit$new(data = weight_per_arm, precache = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "grid")
#'
#'
NULL
