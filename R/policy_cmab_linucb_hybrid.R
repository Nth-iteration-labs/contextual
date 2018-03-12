# ok, some rules: arm specific features always at the end
# and arm specific features are not k, but a ...

# call X feature array, with X arm features, and X_a context features.. (make that even X_c ,and X_ca?)

# could also use full feature vector as ....?

#' @export
LinUCBHybridPolicy <- R6::R6Class(
  "LinUCBHybridPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = NULL,
    initialize = function(alpha = 1.0, name = "LinUCBHybrid") {
      super$initialize(name)
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$theta <- list('A0' = diag(1,self$d,self$d), 'b0' = rep(0,self$d))
      self$theta_to_arms <- list( 'A' = diag(1,self$a_d,self$a_d), 'B' = matrix(0,self$a_d,self$d), 'b' = rep(0,self$a_d))
    },
    get_action = function(context, t) {


      expected_rewards <- rep(0.0, self$k)

      beta_hat <- solve(theta$A0) %*% theta$b0

      for (arm in 1:self$k) {

        A0         <-  theta$A0

        A          <-  theta$A[[arm]]
        B          <-  theta$B[[arm]]
        b          <-  theta$b[[arm]]

        X          <- matrix(context$X[,arm])
        X_a        <- matrix(context$X[(self$x_d + 1):self$d,arm])

        A0_inv <- solve(A0)
        A_inv <- solve(A)

        theta_hat  <-  A_inv %*% (b - B %*% beta_hat)

        sd <- sqrt(  (t(X) %*% A0_inv %*% X) -
                     2*(t(X) %*% A0_inv %*% t(B) %*% A_inv %*% X_a) +
                     (t(X_a) %*% A_inv %*% X_a) +
                     (t(X_a) %*% A_inv %*% B %*% A0_inv %*% t(B) %*% A_inv %*% X_a))

        mean <- (t(X) %*% beta_hat)  +  (t(X_a) %*% theta_hat)


        expected_rewards[arm] <- mean + alpha * sd
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {

      arm            <- action$choice
      reward         <- reward$reward

      X              <- matrix(context$X[,arm])
      X_a            <- matrix(context$X[(self$x_d + 1):self$d,arm])

      A0             <- theta$A0
      b0             <- theta$b0
      A              <- theta$A[[arm]]
      B              <- theta$B[[arm]]
      b              <- theta$b[[arm]]

      ###################################################################

      A_inv          <- solve(A)

      A0             <- A0 + (t(B) %*% A_inv %*% B)
      b0             <- b0 + (t(B) %*% A_inv %*% b)

      A <- A + X_a %*% t(X_a)
      B <- B + X_a %*% t(X)
      b <- b + reward * X_a

      A_inv          <- solve(A)
      A0             <- A0 + (X %*% t(X)) - (t(B) %*% A_inv %*% B)
      b0             <- b0 + (reward * X) - (t(B) %*% A_inv %*% b)

      ####################################################################

      theta$A0       <- A0
      theta$b0       <- b0
      theta$A[[arm]] <- A
      theta$B[[arm]] <- B
      theta$b[[arm]] <- b

      theta
    }
  )
)


#' Policy: LinUCB with hybrid linear models
#'
#' Algorithm 1 LinUCB with hybrid linear models
#' A Contextual-Bandit Approach to
#' Personalized News Article Recommendation
#'
#' Lihong Li et all
#'
#' Each time step t, \code{LinUCBHybridPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name LinUCBHybridPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- LinUCBHybridPolicy(alpha = 1.0, name = "LinUCB")
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
#'   \item{\code{new(alpha = 1, name = "LinUCB")}}{ Generates a new \code{LinUCBHybridPolicy} object. Arguments are defined in the Argument section above.}
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
NULL
