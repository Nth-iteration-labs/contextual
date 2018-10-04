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
          self$theta$gittins_index[[arm]] <- 0    # Gittins index approximation
          private$gittins_approximation(arm)
        }
      } else {
        for (arm in 1:k) {
          self$theta$n[[arm]]             <- sum(self$prior[arm,])
          self$theta$mean[[arm]]          <- self$prior[arm,1]/self$theta$n[[arm]]
          self$theta$gittins_index[[arm]] <- 0
          private$gittins_approximation(arm)
        }
      }
    },
    get_action = function(t, context) {
      action$choice  <- which_max_tied(self$theta$gittins_index)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                         <- action$choice
      reward                      <- reward$reward
      self$theta$n[[arm]]         <- self$theta$n[[arm]] + 1
      self$theta$mean[[arm]]      <- self$theta$mean[[arm]] + (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]
      private$gittins_approximation(arm)
      self$theta
    }
  ),
  private = list(
    gittins_approximation = function(arm) {
      p_hat <- self$theta$mean[[arm]]
      v_arm <- p_hat*(1 - p_hat)
      v_mean <- p_hat*(1 - p_hat)/(self$theta$n[[arm]] + 1)
      c <- - log(self$discount)
      self$theta$gittins_index[[arm]] <- p_hat + sqrt(v_mean) * private$phi(v_mean/(c * v_arm))
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
#' \code{GittinsBrezziLaiPolicy} Algorithm based on Brezzi and Lai (2002)
#' "Optimal learning and experimentation in bandit problems."
#'
#' The algorithm provides an approximation of the Gittins index, by specifying
#' a closed-form expression, which is a function of the discount factor, and
#' the number of successes and failures associated with each arm.
#'
#' @name GittinsBrezziLaiPolicy
#' @aliases gittinsbrezzilai
#'
#' @section Usage:
#' \preformatted{
#'   policy <- GittinsBrezziLaiPolicy$new(discount=0.95, prior=NULL)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{discount}}{
#'      numeric; discount factor
#'   }
#'   \item{\code{prior}}{
#'      numeric matrix; prior beliefs over Bernoulli parameters governing each arm.
#'      Beliefs are specified by Beta distribution with two parameters (alpha,beta)
#'      where alpha = number of success, beta = number of failures.
#'      Matrix is of arms times two (alpha / beta) dimensions
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(discount=0.95, prior=NULL)}}{
#'     Generates and initializes a new \code{Policy} object.
#'   }
#'
#'   \item{\code{get_action(t, context)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'      }
#'      computes which arm to play based on the current values in named list \code{theta}
#'      and the current \code{context}. Returns a named list containing
#'      \code{action$choice}, which holds the index of the arm to play.
#'   }
#'
#'   \item{\code{set_reward(t, context, action, reward)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'          \item \code{reward}:  list, containing \code{reward$reward} and, if available,
#'          \code{reward$optimal} (as set by \code{bandit}).
#'      }
#'    utilizes the above arguments to update and return the set of parameters in list \code{theta}.
#'    }
#'
#'   \item{\code{set_parameters()}}{
#'    Helper function, called during a Policy's initialisation, assigns the values
#'    it finds in list \code{self$theta_to_arms} to each of the Policy's k arms.
#'    The parameters defined here can then be accessed by arm index in the following way:
#'    \code{theta[[index_of_arm]]$parameter_name}.
#'   }
#'
#' }
#'
#' @references
#'
#' Brezzi, M., & Lai, T. L. (2002). Optimal learning and experimentation in bandit problems. Journal of Economic Dynamics and Control, 27(1), 87-108.
#'
#' Implementation follows \url{https://github.com/elarry/bandit-algorithms-simulated}
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
NULL
