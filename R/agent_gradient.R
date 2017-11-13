library(R6)

#' GradientAgent
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
#' \code{$reset()} Resets the bandit$
#'
#' \code{$pull(action)} Returns reward and True if action is optimal.
#'
#' @importFrom R6 R6Class
#' @name GradientAgent
#' @examples
#'
NULL

GradientAgent <- R6Class(

  "GradientAgent",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    bandit = NULL,
    k = NULL,
    prior = NULL,
    gamma = NA,
    alpha = NULL,
    baseline = NULL,
    action_attempts = NULL,
    t = NULL,
    last_action = NULL,
    init_exploration = NULL,
    average_reward = NULL,

    initialize = function(
      bandit = NA,  # remove this in refactor? cmab/mab dif
      policy = NA,
      prior = 0,
      alpha=0.1,
      baseline=TRUE,
      gamma= NA,
      k = NA,
      init_exploration = NA,
      average_reward = 0
    ) {

      self$bandit = bandit
      self$policy = policy

      self$prior = prior
      self$alpha = alpha
      self$baseline = baseline

      self$k = bandit$k
      self$init_exploration = init_exploration

      self$reset()
    },

    reset = function() {
      private$value_estimates = rep(prior, self$k)
      self$action_attempts = rep(0, self$k)
      self$last_action = NA
      self$t = 0
      self$average_reward = 0
    },

    choose = function() {

      action = self$policy$choose(self)
      self$last_action = action
      return(action)
    },

    get_value_estimates = function() {
        return(private$value_estimates)
    },

    observe = function(reward) {

      self$action_attempts[self$last_action] =
        self$action_attempts[self$last_action] + 1

      if (is.na(self$gamma)) {
        g = 1 / self$action_attempts[self$last_action]
      } else {
        g = self$gamma
      }
      q = private$value_estimates[self$last_action]

      private$value_estimates[self$last_action] =
        private$value_estimates[self$last_action] + g * (reward - q)

      self$t = self$t + 1
    }

  ),
  private = list(
    value_estimates = NULL
  )
)
