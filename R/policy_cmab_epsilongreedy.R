#' @export
ContextualEpsilonGreedyPolicy <- R6::R6Class(
  "ContextualEpsilonGreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = NULL,
    epsilon = NULL,
    initialize = function(epsilon = 0.1, alpha = 1.0, name = "ContextualEpsilonGreedyPolicy") {
      super$initialize(name)
      self$alpha <- alpha
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$theta_to_arms <- list( 'A' = diag(1,self$d,self$d), 'b' = rep(0,self$d))
    },
    get_action = function(context, t) {
      expected_rewards <- rep(0.0, context$k)
      X <- context$X
      for (arm in 1:context$k) {

        A          <-  theta$A[[arm]]
        b          <-  theta$b[[arm]]

        A.inv      <-  solve(chol(A))
        theta.hat  <-  A.inv %*% b

        mean       <-  X %*% theta.hat
        variance   <-  sqrt(tcrossprod(context$X %*% A.inv, X))

        expected_rewards[arm] <- mean + alpha * variance
      }
      if (runif(1) > epsilon) {
        action$choice <- max_in(expected_rewards)
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      X <- context$X

      inc(theta$A[[arm]]) <- outer(X, X)
      inc(theta$b[[arm]]) <- reward * X

      theta
    }
  )
)


#' Policy: LinUCB
#'
#' Each time step t, \code{ContextualEpsilonGreedyPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name ContextualEpsilonGreedyPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- ContextualEpsilonGreedyPolicy(alpha = 1.0, name = "LinUCB")
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{alpha}}{
#'    double, a positive real value R+
#'   }
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Parameters:
#'
#' \describe{
#'   \item{\code{A}}{
#'    d*d identity matrix
#'   }
#'   \item{\code{b}}{
#'    a zero vector of length d
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(alpha = 1, name = "LinUCB")}}{ Generates a new \code{ContextualEpsilonGreedyPolicy} object. Arguments are defined in the Argument section above.}
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
#' Li, L., Chu, W., Langford, J., & Schapire, R. E. (2010, April). A contextual-bandit approach to personalized news article recommendation. In Proceedings of the 19th international conference on World wide web (pp. 661-670). ACM.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{AbstractBandit}}, \code{\link{BasicBandit}},
#' \code{\link{OfflineLiBandit}}, \code{\link{SyntheticBandit}}
#'
#'
#'
#'
NULL
