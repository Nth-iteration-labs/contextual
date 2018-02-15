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
    t = NULL,
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
      self$t = 0

      private$state$context <- matrix()
      private$state$action <- list()
      private$state$reward <- list()
      private$theta <- self$policy$initialize_theta()
    },
    step = function() {
      self$t <- self$t + 1
      context <- bandit_get_context()
      action  <- policy_get_action()
      reward  <- bandit_get_reward()
      theta   <- policy_set_reward()
      list(context = context,action = action,reward = reward,theta = theta)
    },
    bandit_get_context = function() {
      private$state$context <- self$bandit$get_context(self$t)
      private$state$context
    },
    policy_get_action = function() {
      self$policy$set_theta(private$theta)
      (private$state$action <- self$policy$get_action(private$state$context))
    },
    bandit_get_reward = function() {
      (private$state$reward <- self$bandit$get_reward(private$state$action,self$t))
    },
    policy_set_reward = function() {
      if (!is.null(private$state$reward))
        (private$theta <- self$policy$set_reward(private$state$reward, private$state$context))
    },
    object_size = function() {
      cat(paste("Agent: ", self$hash),"\n")
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
