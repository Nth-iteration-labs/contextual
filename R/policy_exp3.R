#' @export
Exp3Policy <- R6::R6Class(
  "Exp3Policy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    gamma = NULL,
    initialize = function(gamma =  0.1, name = "Exp3") {
      super$initialize(name)
      self$gamma <- gamma
    },
    set_parameters = function() {
      self$parameters <- list('weight' = 1)
    },
    get_action = function(context) {
      probs <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (theta$weight[[i]] / sum_of(theta$weight))
      }
      inc(probs[i])  <- ((gamma) * (1.0 / context$k))
      action$choice  <- categorical_draw(probs)
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward
      probs  <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (theta$weight[[i]] / sum_of(theta$weight))
         inc(probs[i]) <- gamma  / context$k
      }
      growth_factor <- exp((gamma / context$k) * reward / probs[arm])
      theta$weight[[arm]] <- theta$weight[[arm]] * growth_factor
      theta
    },
    categorical_draw = function(probs) {
      arms = length(probs)
      cumulative_probability <- 0.0
      for (i in 1:arms) {
        inc(cumulative_probability) <- probs[i]
        if ( cumulative_probability > runif(1) ) return(i)
      }
      sample(arms, 1, replace = TRUE)
    }
  )
)

#' Policy: Exp3
#'
#' In \code{Exp3Policy}, "Exp3" stands for "Exponential-weight algorithm for Exploration and Exploitation".
#' It makes use of a distribution over probabilities that is is a mixture of a
#' uniform distribution and a distribution which assigns to each action
#' a probability mass exponential in the estimated cumulative reward for that action.
#'
#' @name Exp3Policy
#' @family contextual classes
#' @family policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- Exp3Policy(gamma = 0.1, name = "Exp3Policy")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{gamma}}{
#'    double, value in the closed interval \code{(0,1]}, controls the exploration - often refered to as the learning rate
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
#'   \item{\code{new(gamma = 0.1, name = "Exp3")}}{ Generates a new \code{Exp3Policy} object. Arguments are defined in the Argument section above.}
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
#' policy             <- Exp3Policy$new(gamma = 0.1, name = "Exp3")
#' bandit             <- SyntheticBandit$new(data = weight_per_arm, precache = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "grid")
#'
#' plot(history, type = "arms")
#'
#'
NULL
