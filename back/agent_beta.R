library(R6)

#' AgentBeta
#'
#' An Agent is able to take one of a set of actions at each time step. The
#' action is chosen using a strategy based on the history of prior actions
#' and outcome observations.
#'
#' @section Usage:
#' \preformatted{p <- process$new(command = NULL, args, commandline = NULL,
#'                  stdout = TRUE, stderr = TRUE)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{k}{An integer: how many arms.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new process, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' \code{$reset()} Resets the bandit.
#'
#' \code{$pull(action)} Returns reward and True if action is optimal.
#'
#' @importFrom R6 R6Class
#' @name Agent
#' @examples
#' test <- AgentBeta$new()
#'
NULL

AgentBeta <- R6Class(

  "AgentBeta",

  inherit = Agent,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    bandit = NULL,
    k = NULL,
    prior = NULL,
    gamma = NULL,
    action_attempts = NULL,
    t = NULL,
    ts = NULL,
    last_action = NULL,
    init_exploration = NULL,

    initialize = function(
                          bandit = NA,  # remove this in refactor? cmab/mab dif
                          policy = NA,
                          prior = 0,
                          gamma = NA,
                          k = NA,
                          init_exploration = NA,
                          ts = TRUE
                          ) {
      self$policy = policy
      self$ts = ts
      self$bandit = bandit
      self$init_exploration = init_exploration
      self$k = bandit.k             # or explicitly set k .. if refactor, see up
      self$prior = prior
      self$gamma = gamma
      self$reset()



    },


    calc_rho = function(i) {
      rbeta(1, self$alpha + self$s_counts[i], self$beta + self$counts[i] - self$s_counts[i])
    },
    get_action = function() {

    },

    reset = function() {
      private$value_estimates = rep(prior, self$k) # Est. Mean reward
      self$action_attempts = rep(0, self$k)
      self$last_action = NA
      self$t = 0
    },

    choose = function() {

        action = self$policy$choose(self)
        self$last_action = action
        return(action)
    },


    observe = function(reward) {

      self$action_attempts[self$last_action] = self$action_attempts[self$last_action] + 1

      if (is.NA(self$gamma)) {
        g = 1 / self$action_attempts[self$last_action]
      } else {
        g = self$gamma
        q = private$value_estimates[self$last_action]
      }

      private$value_estimates[self$last_action] = private$value_estimates[self$last_action] + g * (reward - q)

      self$t = self$t + 1
    }

  ),
  private = list(
    value_estimates = NULL
  )
)

############################################################
best_binomial_bandit <- function(x, n, alpha=1, beta=1) {
  k <- length(x)
  ans <- numeric(k)
  for (i in (1:k)) {
    indx <- (1:k)[-i]
    f <- function(z) {
      r <- dbeta(z, x[i] + alpha, n[i] - x[i] + beta)
      for (j in indx) {
        r <- r * pbeta(z, x[j] + alpha, n[j] - x[j] + beta)
      }
      return(r)
    }
    ans[i] = integrate(f, 0, 1)$value
  }
  return(ans)
}
