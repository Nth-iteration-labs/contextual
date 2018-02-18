#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  private = list(
    theta = NULL,
    state = NULL
  ),
  public = list(
    policy = NULL,
    bandit = NULL,
    sim_index = NULL,
    agent_index = NULL,
    initialize = function(policy, bandit) {
      self$bandit   <- bandit
      self$policy   <- policy

      if (is.null(self$bandit$d)) stop(strwrap(prefix = " ", initial = "",
                                               "No weights have been set - please
                                               set_weights(W) on your Bandit object."),
                                       call. = FALSE)
      self$policy$k <- self$bandit$k
      self$policy$d <- self$bandit$d
      self$reset()
    },
    reset = function() {
      self$policy$set_parameters()
      private$state$t <- 0
      private$state$context <- matrix()
      private$state$action <- list()
      private$state$reward <- list()
      private$theta <- self$policy$initialize_theta()
    },
    set_t = function(t) {
      private$state$t <- t
    },
    step = function() {
      private$state$t <- private$state$t + 1
      list(context = bandit_get_context(),
           action =  policy_get_action(),
           reward =  bandit_get_reward(),
           theta  =  policy_set_reward())
    },
    bandit_get_context = function() {
      private$state$context <- bandit$get_context(private$state$t)
      private$state$context
    },
    policy_get_action = function() {
      policy$set_theta(private$theta)
      private$state$action <- policy$get_action(private$state$context)
      private$state$action
    },
    bandit_get_reward = function() {
      private$state$reward <- bandit$get_reward(private$state$action,private$state$t)
      private$state$reward
    },
    policy_set_reward = function() {
      if (!is.null(private$state$reward)) {
        private$theta <- policy$set_reward(private$state$context, private$state$action, private$state$reward )
        private$theta
      }
    },
    object_size = function() {
      cat(paste("Agent: ", hash),"\n")
      bandit$object_size()
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
