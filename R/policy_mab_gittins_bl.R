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
      self$discount                     <- discount
      self$prior                        <- prior
    },
    set_parameters = function(context_params) {

      k <- context_params$k

      if(is.null(self$prior)) {
        for (arm in 1:k) {
          self$theta$n[[arm]]             <- 2
          self$theta$mean[[arm]]          <- 0.5
          self$theta$gittins_index[[arm]] <- 0
          self$gittins_approximation(arm)
        }
      } else {
        for (arm in 1:k) {
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
      v_mean <- p_hat*(1 - p_hat)/(self$theta$n[[arm]] + 1)
      c <- - log(self$discount)
      self$theta$gittins_index[[arm]] <- p_hat + sqrt(v_mean) * self$phi(v_mean/(c * v_arm))
    },
    phi = function(s) {
      if (s > 15) {
        (2*log(s) - log(log(s)) - log(16*pi))^0.5         # nocov
      } else if (5 < s && s <= 15) {
        0.77 - 0.58*s^(-0.5)
      } else if (1 < s && s <= 5) {
        0.63 - 0.26*s^(-0.5)
      } else if (0.2 < s && s <= 1) {
        0.49 - 0.11*s^(-0.5)
      } else if (0 <= s && s <= 0.2) {
        sqrt(s/2.0)
      } else {
        message('Domain error in Brezzi_Lai Phi-function') # nocov
        0.0                                                # nocov
      }
    }
  )
)

#' Policy: Gittins Approximation algorithm for choosing arms in a MAB problem.
#'
#' \code{GittinsBrezziLaiPolicy} Algorithm based on Brezzi and Lai (2002) "Optimal learning
#' and experimentation in bandit problems."
#' The algorithm provides an approximation of the Gittins index, by specifying
#' a closed-form expression, which is a function of the discount factor, and
#' the number of successes and failures associated with each arm.
#'
#' @name GittinsBrezziLaiPolicy
#' @family contextual subclasses
#'
#'
NULL
