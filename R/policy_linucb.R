#' @export
LinUCBPolicy <- R6::R6Class(
  "LinUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = NULL,
    initialize = function(alpha = 1.0, name = "LinUCB") {
      super$initialize(name)
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$parameters <- list( 'A' = diag(1,self$d), 'b' = rep(0,self$d))
    },
    get_action = function(context) {
      expected_rewards <- rep(0.0, context$k)
      X <- context$X
      for (arm in 1:context$k) {
        A          <-  theta$A[[arm]]
        b          <-  theta$b[[arm]]
        A.inv      <-  chol2inv(chol(A))
        theta.hat  <-  A.inv %*% b
        mean       <-  X %*% theta.hat
        var        <-  sqrt(tcrossprod(context$X %*% A.inv, X))
        expected_rewards[arm] <- mean + (alpha * var)
      }
      action$arm  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward) {
      arm <- action$arm
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
#' Each time step t, \code{LinUCBPolicy} runs a linear regression that produces coefficients for each context feature \code{d}.
#' It then observes the new context, and generates a predicted payoff or reward together with a confidence interval for each available arm.
#' It then proceeds to choose the arm with the highest upper confidence bound.
#'
#' @name LinUCBPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- LinUCBPolicy(alpha = 1.0, name = "LinUCB")
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
#'   \item{\code{new(alpha = 1, name = "LinUCB")}}{ Generates a new \code{LinUCBPolicy} object. Arguments are defined in the Argument section above.}
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
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weight_per_arm     <- c(0.9, 0.1, 0.1)
#'
#' policy             <- LinUCBPolicy$new(alpha = 1.0, name = "LinUCB")
#' bandit             <- SyntheticBandit$new(weights = weight_per_arm, precache = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "grid")
#'
#'
NULL
