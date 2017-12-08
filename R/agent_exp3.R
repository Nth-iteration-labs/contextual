#' @export
Exp3Agent <- R6::R6Class(
  "Exp3Agent",
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
      private$memory$theta = rep(1.0, self$bandit$k)
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
      probs = rep(0.0, self$bandit$k)
      for (arm in 1:self$bandit$k) {
        probs[arm] = (1 - self$policy$gamma) * (private$memory$theta[arm] / sum(private$memory$theta))
        probs[arm] = probs[arm] + (self$policy$gamma) * (1.0 / self$bandit$k)
      }
      x = reward$reward / probs[reward$current_choice]
      growth_factor = exp((self$policy$gamma / self$bandit$k) * x)
      private$memory$theta[reward$current_choice] <- private$memory$theta[reward$current_choice] * growth_factor
    }
  )
)

#' External Exp3Agent
#'
#' Exp3Agent intro
#'
#' @section Usage:
#' \preformatted{b <- Exp3Agent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Exp3Agent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Exp3Agent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Exp3Agent
#' @examples
#'\dontrun{}
#'
NULL
