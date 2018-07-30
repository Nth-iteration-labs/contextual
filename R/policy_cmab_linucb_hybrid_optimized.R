#' @export
LinUCBHybridOptimizedPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "LinUCBHybridOptimizedPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha <- alpha
    },
    set_parameters = function(context_params) {
      ul                 <- length(context_params$unique)
      sl                 <- length(context_params$unique) * length(context_params$shared)
      self$theta         <- list( 'A0' = diag(1,sl,sl), 'A0_inv' = diag(1,sl,sl),
                                  'b0' = rep(0,sl),'z' = matrix(0,ul,ul), 'x' = rep(0,ul))
      self$theta_to_arms <- list( 'A' = diag(1,ul,ul), 'A_inv' = diag(1,ul,ul),
                                  'B' = matrix(0,ul,sl), 'b' = rep(0,ul))
    },
    get_action = function(t, context) {
      expected_rewards <- rep(0.0, context$k)

      A0_inv     <- self$theta$A0_inv
      b0         <- self$theta$b0

      beta_hat   <- A0_inv %*% b0

      for (arm in 1:context$k) {

        ################## unpack thetas ##############################################

        A          <- self$theta$A[[arm]]
        A_inv      <- self$theta$A_inv[[arm]]
        B          <- self$theta$B[[arm]]
        b          <- self$theta$b[[arm]]
        x          <- context$X[context$unique,arm]
        z          <- matrix(as.vector(outer(x,context$X[context$shared,arm])))


        ################## compute expected reward per arm #############################

        theta_hat  <- A_inv %*% (b - B %*% beta_hat)

        tBAinvx <- crossprod(B, (A_inv %*% x))
        txAinv  <- crossprod(x, A_inv)
        tzA0inv  <-crossprod(z, A0_inv)

        sd <- sqrt(
          (tzA0inv %*% z) - 2*(tzA0inv %*% tBAinvx) +
            txAinv %*% x + (txAinv %*% B) %*% (A0_inv %*% tBAinvx)
        )

        mean <- crossprod(z, beta_hat)  +  crossprod(x, theta_hat)

        expected_rewards[arm] <- mean + self$alpha * sd
      }



      ################## choose arm with highest expected reward #######################

      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {

      #################### unpack thetas ###############################################

      arm            <- action$choice
      reward         <- reward$reward
      x              <- context$X[context$unique,arm]
      z              <- matrix(as.vector(outer(x,context$X[context$shared,arm])))

      A0             <- self$theta$A0
      A0_inv         <- self$theta$A0_inv
      b0             <- self$theta$b0
      A              <- self$theta$A[[arm]]
      A_inv          <- self$theta$A_inv[[arm]]
      B              <- self$theta$B[[arm]]
      b              <- self$theta$b[[arm]]

      #################### update thetas with returned reward & arm choice #############

      BAinv          <- crossprod(B, A_inv)

      A0             <- A0 + (BAinv %*% B)
      b0             <- b0 + (BAinv %*% b)

      A              <- A + x %*% t(x)
      B              <- B + x %*% t(z)
      b              <- b + reward * x

      A_inv          <- sherman_morrisson(A_inv,as.vector(x))

      tBAinv         <- crossprod(B, A_inv)

      A0             <- A0 + tcrossprod(z,z) - (tBAinv %*% B)
      b0             <- b0 + (reward * z) - (tBAinv %*% b)

      A0_inv         <- inv(A0)

      #################### pack thetas ################################################

      self$theta$A0_inv       <- A0_inv

      self$theta$A0           <- A0
      self$theta$b0           <- b0
      self$theta$A[[arm]]     <- A
      self$theta$A_inv[[arm]] <- A_inv
      self$theta$B[[arm]]     <- B
      self$theta$b[[arm]]     <- b

      self$theta
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
#' Each time step t, \code{LinUCBHybridOptimizedPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name LinUCBHybridOptimizedPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- LinUCBHybridOptimizedPolicy(alpha = 1.0)
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
#'   \item{\code{new(alpha = 1)}}{ Generates a new \code{LinUCBHybridOptimizedPolicy} object. Arguments are defined in the Argument section above.}
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
#'
NULL
