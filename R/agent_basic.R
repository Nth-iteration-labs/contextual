#' @export
BasicAgent <- R6::R6Class(
  "BasicAgent",
  private = list(.memory = list()),
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
        'chosen' = 0,    # number choices
        'succesful' = 0, # number of succesful
        'value' = 0      # theta main value -> refactor this!!!!!!
      )
      for (arm in 1:self$bandit$k) {
        private$.memory$theta[[arm]] <- theta.arm
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

      private$.memory$theta[[reward$choice]]$chosen <-
        private$.memory$theta[[reward$choice]]$chosen + 1

      if (reward$reward == 1)
        private$.memory$theta[[reward$choice]]$succes <-                         ### make this more into SB!!!
          private$.memory$theta[[reward$choice]]$succes + 1

      private$.memory$theta[[reward$choice]]$value <-
        private$.memory$theta[[reward$choice]]$value +
        (1 / private$.memory$theta[[reward$choice]]$chosen) *
        (reward$reward - private$.memory$theta[[reward$choice]]$value)

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
