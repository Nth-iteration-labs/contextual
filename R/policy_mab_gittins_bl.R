#Prior must be of dimensions: *n_arms*, 2

#' @export
GittinsBrezziLaiPolicy          <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    discount = NULL,
    prior = NULL,
    class_name = "GittinsBrezziLaiPolicy",
    initialize = function(discount=0.95, prior=NULL) {
      super$initialize()
      self$discount                  <- discount
      self$prior                     <- prior
    },
    set_parameters = function() {
      if(is.null(self$prior)) {
        for (arm in 1:self$k) {
          self$theta$n[[arm]]             <-  2    #1&1
          self$theta$mean[[arm]]          <-  0.5  #1/(1+1)
          self$theta$gittins_index[[arm]] <-  0
          self$gittins_approximation(arm)
        }
      } else {
        for (arm in 1:self$k) {
          self$theta$n[[arm]]             <- sum(self$prior[arm,])
          self$theta$mean[[arm]]          <- self$prior[arm,1]/self$theta$n[[arm]]
          self$theta$gittins_index[[arm]] <- 0
          self$gittins_approximation(arm)
        }
      }
    },
    get_action = function(t, context) {
      action$choice  <- max_in(self$theta$gittins_index)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                         <- action$choice
      reward                      <- reward$reward
      self$theta$n[[arm]]         <- self$theta$n[[arm]] + 1
      self$theta$mean[[arm]]      <- self$theta$mean[[arm]] + (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      self$gittins_approximation(arm)
      self$theta
    },
    gittins_approximation = function(arm) {
      p_hat <- self$theta$mean[[arm]]
      v_arm <- p_hat*(1 - p_hat)
      v_mean = p_hat*(1 - p_hat)/(self$theta$n[[arm]] + 1)
      c = - log(self$discount)
      self$theta$gittins_index[[arm]]= p_hat + sqrt(v_mean) * self$phi(v_mean/(c * v_arm))
    },
    phi = function(s) {
      if (s > 15) {
        (2*log(s) - log(log(s)) - log(16*pi))^0.5
      } else if (5 < s && s <= 15) {
        0.77 - 0.58*s^(-0.5)
      } else if (1 < s && s <= 5) {
        0.63 - 0.26*s^(-0.5)
      } else if (0.2 < s && s <= 1) {
        0.49 - 0.11*s^(-0.5)
      } else if (0 <= s && s <= 0.2) {
        sqrt(s/2.0)
      } else {
        message('Domain error in Brezzi_Lai Phi-function')
        0.0
      }
    }
  )
)

#' Policy: Epsilon Greedy
#'
#' \code{GittinsBrezziLaiPolicy} chooses an arm at
#' random (explores) with probability \code{epsilon}, otherwise it
#' greedily chooses (exploits) the arm with the highest estimated
#' reward.
#'
#' @name GittinsBrezziLaiPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- GittinsBrezziLaiPolicy(epsilon = 0.1)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{epsilon}}{
#'    double, value in the closed interval \code{(0,1]} indicating the probablilty with which
#'    arms are selected at random (explored).
#'    Otherwise, \code{GittinsBrezziLaiPolicy} chooses the best arm (exploits)
#'    with a probability of \code{1 - epsilon}
#'
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
#'   \item{\code{new(epsilon = 0.1)}}{ Generates a new \code{GittinsBrezziLaiPolicy} object. Arguments are defined in the Argument section above.}
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
#' Gittins, J., Glazebrook, K., & Weber, R. (2011). Multi-armed bandit allocation indices. John Wiley & Sons. (Original work published 1989)
#'
#' Sutton, R. S. (1996). Generalization in reinforcement learning: Successful examples using sparse coarse coding. In Advances in neural information processing systems (pp. 1038-1044).
#'
#' Strehl, A., & Littman, M. (2004). Exploration via modelbased interval estimation. In International Conference on Machine Learning, number Icml.
#'
#' Yue, Y., Broder, J., Kleinberg, R., & Joachims, T. (2012). The k-armed dueling bandits problem. Journal of Computer and System Sciences, 78(5), 1538-1556.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#'
#' @examples
#'
#' horizon            <- 100L
#' simulations        <- 100L
#' weights            <- c(0.9, 0.1, 0.1)
#'
#' policy             <- GittinsBrezziLaiPolicy$new(epsilon = 0.1)
#' bandit             <- SyntheticBandit$new(weights = weights, precaching = FALSE)
#' agent              <- Agent$new(policy, bandit)
#'
#' history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()
#'
#' plot(history, type = "cumulative")
#'
#' plot(history, type = "arms")
#'
#'
NULL
