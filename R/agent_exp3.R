#' @export
Exp3Agent <- R6::R6Class(
  "Exp3Agent",
  inherit = Contextual,
  private = list(.memory = NULL),
  active = list(
    memory = function(value) {
      if (missing(value)) {
        private$.memory
      } else {
        stop("'$memory' is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(policy,
                          bandit) {
      stopifnot(is.element("R6", class(policy)))
      stopifnot(is.element("R6", class(bandit)))
      self$bandit <- bandit
      self$policy <- policy
      self$reset()
    },
    reset = function() {
      theta.arm <- list(
        'value' = 1
      )
      for (i in 1:self$bandit$k) {
        private$.memory$theta[[i]] <- theta.arm
      }
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
      probs <- rep(0.0, self$bandit$k)
      for (arm in 1:self$bandit$k) {
        probs[arm] = (1 - self$policy$gamma) *
          (self$memory$theta[[arm]]$value /
             self$sumval(self$memory$theta, "value"))
        probs[arm] = probs[arm] + (self$policy$gamma) * (1.0 / self$bandit$k)
      }
      x <- reward$reward / probs[reward$choice]
      growth_factor <- exp((self$policy$gamma / self$bandit$k) * x)
      private$.memory$theta[[reward$choice]]$value <-
        private$.memory$theta[[reward$choice]]$value * growth_factor
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
