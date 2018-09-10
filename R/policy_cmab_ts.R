#' @export
ContextualThompsonSamplingPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    v = NULL,
    delta = NULL,
    R = NULL,
    epsilon = NULL,

    class_name = "ContextualThompsonSamplingPolicy",
    initialize = function(delta=0.5, R=0.5, epsilon=0.2) {
      super$initialize()
      self$delta   <- delta
      self$R       <- R
      self$epsilon <- epsilon
      self$v       <- v
    },
    set_parameters = function(context_params) {
      d <- context_params$d
      if(is.null(self$v)) {
          self$v     <- self$R * sqrt(24 / self$epsilon * d * log(1 /self$delta))
      }
      self$theta     <- list( 'B'  = diag(1, d, d), 'f'  = rep(0, d), 'mu_hat' = rep(0, d))
    },
    get_action = function(t, context) {
      X <- context$X
      mu_tilde <- contextual::mvrnorm(1, self$theta$mu_hat, self$v^2 * solve(self$theta$B))
      expected_rewards <- mu_tilde %*% X
      action$choice <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      reward <- reward$reward
      arm    <- action$choice
      Xa      <- context$X[,arm]
      inc(self$theta$B)    <- Xa %*% t(Xa)
      inc(self$theta$f)    <- reward * Xa
      self$theta$mu_hat    <- inv(self$theta$B ) %*% self$theta$f
      self$theta
    }
  )
)

#' Policy: Contextual Thompson Sampling with Linear Payoffs
#'
#' \code{ContextualThompsonSamplingPolicy} implements Thompson Sampling with Linear
#' Payoffs, following Agrawal and Goyal (2011).
#' Thompson Sampling with Linear Payoffs is a contextual multi-armed bandit
#' algorithm which assumes the underlying relationship between rewards and contexts
#' is linear. Check the reference for more details.
#'
#' @name ContextualThompsonSamplingPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualThompsonSamplingPolicy$new(delta, R, epsilon)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{delta}}{
#'    numeric, 0 < \code{delta} <= 1.
#'    With probability 1 - delta, ContextualThompsonSamplingPolicy satisfies the theoretical regret bound.
#'   }
#'   \item{\code{R}}{
#'    numeric. \code{R} >= 0.
#'    Assume that the residual  \eqn{ri(t) - bi(t)^T \hat{\mu}} is R-sub-gaussian.
#'    In this case, \eqn{R^2} represents the variance for residuals of the linear model \eqn{bi(t)^T}.
#'    }
#'   \item{\code{epsilon}}{
#'    numeric. 0 < \code{epsilon} < 1
#'    If the total trials T is known, we can choose epsilon = 1/ln(T).
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(...)}}{ Instantiates a new \code{ContextualThompsonSamplingPolicy} instance.
#'      Arguments defined in the Arguments section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters(context_params)}}{
#'      Initialization of policy parameters, utilising \code{context_params$k} (number of arms) and
#'      \code{context_params$d} (number of context features).
#'   }
#' }
#'
#' \describe{
#'   \item{\code{get_action(t,context)}}{
#'     Selects an arm based on \code{self$theta} and \code{context}, returning the index of the selected arm
#'     in \code{action$choice}. The {context} argument consists of a list with \code{context$k} (number of arms),
#'     \code{context$d} (number of features), and the feature matrix \code{context$X} with dimensions
#'     \eqn{d \times k}{d x k}.
#'    }
#'   }
#'
#'  \describe{
#'   \item{\code{set_reward(t, context, action, reward)}}{
#'     Updates parameter list \code{theta} in accordance with the current \code{reward$reward},
#'     \code{action$choice} and the feature matrix \code{context$X} with dimensions
#'     \eqn{d \times k}{d x k}. Returns the updated \code{theta} instance.
#'    }
#'   }
#'
#' @references
#'
#' Shipra Agrawal, and Navin Goyal. "Thompson Sampling for Contextual Bandits with Linear Payoffs." Advances in Neural Information Processing Systems 24. 2011.
#'
#' Implementation follows linthompsamp from https://github.com/ntucllab/striatum
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#' @examples
#' \donttest{
#' horizon            <- 1000L
#' simulations        <- 10L
#'
#' bandit             <- ContextualBasicBandit$new(k = 5, d = 5)
#'
#' delta              <- 0.5
#' R                  <- 0.01
#' epsilon            <- 0.5
#'
#' policy             <- ContextualThompsonSamplingPolicy$new(delta, R, epsilon)
#'
#' agent              <- Agent$new(policy, bandit)
#'
#' simulation         <- Simulator$new(agents, horizon, simulations)
#' history            <- simulation$run()
#'
#' plot(history, type = "cumulative", regret = FALSE, rate = TRUE)
#' }
NULL


