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


    #delta: float, 0 < delta < 1
    #With probability 1 - delta, LinThompSamp satisfies the theoretical
    #regret bound.

    #R: float, R >= 0
    #Assume that the residual  :math:`ri(t) - bi(t)^T \hat{\mu}`
    #is R-sub-gaussian. In this case, R^2 represents the variance for
    #residuals of the linear model :math:`bi(t)^T`.

    #epsilon: float, 0 < epsilon < 1
    #A  parameter  used  by  the  Thompson Sampling algorithm.
    #If the total trials T is known, we can choose epsilon = 1/ln(T).

    class_name = "ContextualThompsonSamplingPolicy",
    initialize = function(delta=0.5, R=0.5, epsilon=0.2, v=NULL) {
      super$initialize()
      self$delta   <- delta
      self$R       <- R
      self$epsilon <- epsilon
      self$epsilon <- epsilon
      self$v       <- v

    },
    set_parameters = function(context_params) {
      d <- context_params$d
      if(is.null(self$v)) {
          self$v     <- self$R * sqrt(24 / self$epsilon * d * log(1 /self$delta))
      }
      self$theta  <- list( 'B'  = diag(1, d, d), 'f'  = rep(0, d), 'mu_hat' = rep(0, d))
    },
    get_action = function(t, context) {
      X <- context$X
      mu_tilde <- self$mvrnorm(1, self$theta$mu_hat, self$v^2 * solve(self$theta$B))
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
    },
    mvrnorm = function(n, mu, sigma)
    {
      ncols <- ncol(sigma)
      mu <- rep(mu, each = n)
      mu + matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
    }
  )
)
# B: the estimated covariance matrix of normal distribution B^(-1)
# mu_hat: vector of the estimated mu_hat vector of normal distribution (posterior)
# f: cumulative selected contextual vector with reward (dimension of contextual vector*1)

#' Policy: Contextual Thompson Sampling with Linear Payoffs
#'
#' \code{ContextualThompsonSamplingPolicy} works by maintaining a prior on the the mu_hat rewards of its arms.
#' In this, it follows a beta–binomial model with parameters alpha and beta, sampling values
#' for each arm from its prior and picking the arm with the highest value.
#' When an arm is pulled and a Bernoulli reward is observed, it modifies the prior based on the reward.
#' This procedure is repeated for the next arm pull.
#'
#' @name ContextualThompsonSamplingPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualThompsonSamplingPolicy(alpha = 1, beta = 1)
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
#'   \item{\code{new(alpha = 1, beta = 1)}}{ Generates a new \code{ContextualThompsonSamplingPolicy} object. Arguments are defined in the Argument section above.}
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
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#'
NULL
