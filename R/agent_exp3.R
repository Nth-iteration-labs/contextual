library(R6)
#' @export
Exp3Agent <- R6Class(
  "Exp3Agent",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(policy,
                          bandit) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    get_memory = function() {
      return(private$memory)
    },
    reset = function() {
      private$memory$theta = rep(1.0, self$bandit$k)
    },
    get_action = function(context = NULL) {
      return(self$policy$get_action(self))
    },
    set_reward = function(reward, context = NULL) {
      probs = rep(0.0, self$bandit$k)
      for (arm in 1:self$bandit$k) {
        probs[arm] = (1 - self$policy$gamma) *
          (private$memory$theta[arm] / sum(private$memory$theta))
        inc(probs[arm]) <-
          (self$policy$gamma) * (1.0 / self$bandit$k)
      }
      x = reward$reward / probs[reward$current_choice]
      growth.factor = exp((self$policy$gamma / self$bandit$k) * x)
      mult(private$memory$theta[reward$current_choice]) <-
        growth.factor
    }
  ),
  private = list(memory = NULL)
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
