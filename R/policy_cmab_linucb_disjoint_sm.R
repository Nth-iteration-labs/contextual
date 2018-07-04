library(Matrix)


#' @export
LinUCBDisjointSmPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "LinUCBDisjointSmPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms <- list( 'A' = diag(1,self$d,self$d), 'b' = rep(0,self$d),
                                  'A_inv' = solve(diag(1,self$d,self$d)))

    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, context$k)

      for (arm in 1:self$k) {

        X          <-  context$X[,arm]
        A          <-  self$theta$A[[arm]]
        A_inv      <-  self$theta$A_inv[[arm]]
        b          <-  self$theta$b[[arm]]

        theta_hat  <-  A_inv %*% b

        mean       <-  X %*% theta_hat
        sd         <-  sqrt(tcrossprod(X %*% A_inv, X))

        expected_rewards[arm] <- mean + self$alpha * sd
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      Xa     <- context$X[,arm]
      A_inv  <-  self$theta$A_inv[[arm]]

      # Sherman-Morrison inverse
      outer_Xa <- outer(Xa, Xa)
      self$theta$A_inv[[arm]]  <- A_inv - c((A_inv %*% (outer_Xa %*% A_inv))) / c(1.0+ (crossprod(Xa,A_inv) %*% Xa))

      self$theta$A[[arm]]      <- self$theta$A[[arm]] + outer_Xa
      self$theta$b[[arm]]      <- self$theta$b[[arm]] + reward * Xa

      self$theta
    }
  )
)


#' Policy: LinUCB with disjoint linear models
#'
#' Algorithm 1 LinUCB with disjoint linear models
#' A Contextual-Bandit Approach to
#' Personalized News Article Recommendation
#'
#' Lihong Li et all
#'
#' Each time step t, \code{LinUCBDisjointSmPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name LinUCBDisjointSmPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- LinUCBDisjointSmPolicy(alpha = 1.0)
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
#'   \item{\code{new(alpha = 1)}}{ Generates a new \code{LinUCBDisjointSmPolicy} object. Arguments are defined in the Argument section above.}
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
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#'
NULL
