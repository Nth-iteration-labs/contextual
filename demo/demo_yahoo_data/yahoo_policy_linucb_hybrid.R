#' @export
YahooLinUCBHybridPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    u = NULL,
    dd = NULL,
    # TODO: rename grouping to disjoint and conjoint, or shared and unshared, default NULL, but can be 1:6, 1:10 etc
    class_name = "YahooLinUCBHybridPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha <- alpha
    },
    set_parameters = function() {
      dd = self$d*self$d
      d  = self$d
      self$theta <- list('A0' = diag(1,dd,dd), 'A0_inv' = diag(1,dd,dd), 'b0' = rep(0,dd))
      self$theta_to_arms <- list( 'A' = diag(1,d,d), 'A_inv' = diag(1,d,d),
                                  'B' = matrix(0,d,dd), 'b' = rep(0,d))
    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, length(context$arms))

      local_arms       <- context$arms

      A0_inv     <-  self$theta$A0_inv
      b0         <-  self$theta$b0

      beta_hat   <- A0_inv %*% b0




      for (arm in seq_along(local_arms)) {



        ################## unpack thetas ##############################################

        A          <-  self$theta$A[[local_arms[arm]]]
        A_inv      <-  self$theta$A_inv[[local_arms[arm]]]
        B          <-  self$theta$B[[local_arms[arm]]]
        b          <-  self$theta$b[[local_arms[arm]]]
        z          <-  matrix(as.vector(outer(context$X[7:12,arm],context$X[1:6,arm])))
        x          <-  matrix(context$X[7:12,arm])

        ################## compute expected reward per arm #############################

        theta_hat  <-  A_inv %*% (b - B %*% beta_hat)

        sd <- sqrt(  (crossprod(z, A0_inv) %*% z) -
                       2*((crossprod(z,A0_inv) %*% crossprod(B,A_inv)) %*% x) +
                       (crossprod(x ,A_inv) %*% x) +
                       (((crossprod(x, A_inv) %*% (B %*% A0_inv)) %*% crossprod(B, A_inv)) %*% x))

        mean <- crossprod(z, beta_hat)  +  crossprod(x, theta_hat)

        expected_rewards[arm] <- mean + alpha * sd
      }

      ################## choose arm with highest expected reward #######################

      action$choice  <- context$arms[max_in(expected_rewards)]
      action
    },
    set_reward = function(t, context, action, reward) {

      #################### unpack thetas ###############################################

      arm            <- action$choice
      arm_index      <- which(context$arms == arm)
      reward         <- reward$reward

      z              <- matrix(as.vector(outer(context$X[7:12,arm],context$X[1:6,arm])))
      x              <- matrix(context$X[7:12,arm_index])


      A0             <- self$theta$A0
      A0_inv         <- self$theta$A0_inv
      b0             <- self$theta$b0
      A              <- self$theta$A[[arm]]
      A_inv          <- self$theta$A_inv[[arm]]
      B              <- self$theta$B[[arm]]
      b              <- self$theta$b[[arm]]

      #################### update thetas with returned reward & arm choice #############

      A0             <- A0 + (t(B) %*% A_inv %*% B)
      b0             <- b0 + (t(B) %*% A_inv %*% b)

      A              <- A + x %*% t(x)
      B              <- B + x %*% t(z)
      b              <- b + reward * x

      A_inv          <- sherman_morrisson(A_inv,as.vector(x))

      A0             <- A0 + (z %*% t(z)) - (t(B) %*% A_inv %*% B)
      b0             <- b0 + (reward * z) - (t(B) %*% A_inv %*% b)

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
#' Each time step t, \code{YahooLinUCBHybridPolicy} runs a linear regression per arm that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name YahooLinUCBHybridPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- YahooLinUCBHybridPolicy(alpha = 1.0)
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
#'   \item{\code{new(alpha = 1)}}{ Generates a new \code{YahooLinUCBHybridPolicy} object. Arguments are defined in the Argument section above.}
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
