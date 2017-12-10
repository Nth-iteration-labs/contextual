#' @export
AbstractAgent <- R6::R6Class(
  "AbstractAgent",
  portable = FALSE,
  inherit = Contextual,
  private = list(
    .theta = NULL,
    .state = NULL,
    initiate_theta = function(t=NA) {
      theta_arm <- list('value' = 0)
      for (arm in 1:self$bandit$k) private$.theta[[arm]] <- theta_arm
    }
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
    initialize = function(policy,
                          bandit) {
      stopifnot(is.element("R6", class(policy)))
      stopifnot(is.element("R6", class(bandit)))
      self$bandit <- bandit
      self$policy <- policy
      self$reset()
    },
    reset = function() {
      private$.state$context <- matrix
      private$.state$action <- list()
      private$.state$reward <- list()
      private$.state$t <- 0
      private$initiate_theta()
    },
    observe_bandit = function(t=NA) {
      if (is.na(t)) {
        private$.state$t <- t + 1
      } else {
        private$.state$t <- t
      }
      private$.state$context <- self$bandit$get_context()
      private$.state$context
    },
    get_policy_decision = function(t=NA) {
      private$.state$action <- self$policy$get_action(private$.state$context,
                                                     private$.theta)
      private$.state$action
    },
    get_bandit_reward = function(t=NA) {
      private$.state$reward <-
        self$bandit$get_reward(private$.state$action)
      private$.state$reward
    },
    adjust_policy = function(t=NA) {
      private$.theta <- self$policy$set_reward(private$.state$reward,
                                               private$.state$context,
                                               private$.theta)
      private$.theta
    }
  )
)

#' External AbstractAgent
#'
#' AbstractAgent intro
#'
#' @section Usage:
#' \preformatted{b <- AbstractAgent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{AbstractAgent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new AbstractAgent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name AbstractAgent
#' @examples
#'\dontrun{}
#'
NULL
