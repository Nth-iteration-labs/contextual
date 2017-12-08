#' @export
BasicAgent <- R6::R6Class(
  "BasicAgent",
  private = list(memory = NULL),
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(policy,
                          bandit) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    reset = function() {
      private$memory$theta =         rep(0, self$bandit$k)                      # theta per arm
      private$memory$choice.counts = rep(0, self$bandit$k)                      # per arm count
      private$memory$succes.counts = rep(0, self$bandit$k)                      # per arm succesful count
    },
    get_memory = function() {
      private$memory
    },
    get_context = function() {
      self$bandit$get_context()
    },
    get_action = function(context) {
      self$policy$get_action(self, context)
    },
    get_reward = function(action) {
      self$bandit$get_reward(action)
    },
    set_reward = function(reward, context = NULL) {
      private$memory$choice.counts[reward$current_choice] = private$memory$choice.counts[reward$current_choice] + 1
      if (reward$reward == 1) private$memory$succes.counts[reward$current_choice] = private$memory$succes.counts[reward$current_choice] + 1
      private$memory$theta[reward$current_choice] = private$memory$theta[reward$current_choice] +
        (1 / private$memory$choice.counts[reward$current_choice]) *
        (reward$reward - private$memory$theta[reward$current_choice])

    }
  )
)

#' External BasicAgent
#'
#' BasicAgent intro
#'
#' @section Usage:
#' \preformatted{b <- BasicAgent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{BasicAgent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new BasicAgent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name BasicAgent
#' @examples
#'\dontrun{}
#'
NULL
