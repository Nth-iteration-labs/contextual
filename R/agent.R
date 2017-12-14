#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  inherit = Contextual,
  private = list(
    .theta = NULL,
    .state = NULL
  ),
  active = list(
    theta = function(value) {
      if (missing(value)) {
        private$.theta
      } else {
        stop("'$theta' is read only", call. = FALSE)
      }
    },
    state = function(value) {
      if (missing(value)) {
        private$.state
      } else {
        stop("'$state' is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(policy, bandit) {
      stopifnot(is.element("R6", class(policy)))
      stopifnot(is.element("R6", class(bandit)))
      self$bandit <- bandit$clone()
      self$policy <- policy$clone()
      self$policy$k = self$bandit$k
      self$policy$d = self$bandit$d
      self$reset()
    },
    reset = function() {
      self$policy$set_parameters()
      private$.theta = self$policy$initialize_theta()
      private$.state$context <- matrix()
      private$.state$action <- list()
      private$.state$reward <- list()
      private$.state$t <- 0
    },
    bandit_get_context = function(t=NA) {
      if (is.na(t)) {
        private$.state$t <- t + 1
      } else {
        private$.state$t <- t
      }
      private$.state$context <- self$bandit$get_context()
      self$policy$k = private$.state$context$k
      self$policy$d = private$.state$context$d
      private$.state$context
    },
    policy_get_decision = function(t=NA) {
      self$policy$set_theta(private$.theta)
      private$.state$action <- self$policy$get_action(private$.state$context)
      private$.state$action
    },
    bandit_get_reward = function(t=NA) {
      private$.state$reward <- self$bandit$get_reward(private$.state$action)
      private$.state$reward
    },
    policy_set_reward = function(t=NA) {
      private$.theta <- self$policy$set_reward(private$.state$reward, private$.state$context)
      private$.theta
    }
  )
)

#' External Agent
#'
#' Agent intro
#'
#' @section Usage:
#' \preformatted{b <- Agent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Agent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Agent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Agent
#' @examples
#'\dontrun{}
#'
NULL
