library(R6)

#' Agent
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
#' test <- Agent$new()
#'
NULL

Agent <- R6Class(

  "Agent",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    k = NULL,
    prior = NULL,
    gamma = NULL,
    action_attempts = NULL,
    t = NULL,
    last_action = NULL,
    init_exploration = NULL,

    initialize = function(
                          policy = NA,
                          prior = 0,
                          gamma = NA,
                          init_exploration = NA,
                          k = NA
                          ) {
      self$policy = policy
      self$init_exploration = init_exploration
      self$k = k
      self$prior = prior
      self$gamma = gamma

      self$reset()



    },

    reset = function() {
      private$value_estimates = rep(prior, self$k) # Est. Mean reward
      self$action_attempts = rep(0, self$k)
      self$last_action = NA
      self$t = 0
    },

    choose = function() {
      if (self$t < self$init_exploration) {
        action = sample(1:self$k, 1)
      } else {
        action = self$policy$choose(self)
        self$last_action = action
        return(action)
      }
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
