#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  class = FALSE,
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
    }
  ),
  public = list(
    policy = NULL,
    bandit = NULL,
    s_index = NULL,
    a_index = NULL,
    log_socket = NULL,
    initialize = function(policy, bandit) {
      super$initialize()
      self$bandit   <- bandit                                                   ## to clone, or not to clone
      self$policy   <- policy                                                   ## that is the question..
      self$policy$k <- self$bandit$k
      self$policy$d <- self$bandit$d
      self$log_socket <- make.socket(port = 4000)
      self$reset()
    },
    reset = function() {
      self$policy$set_parameters()
      private$.theta = self$policy$initialize_theta()
      private$.state$context <- matrix()
      private$.state$action <- list()
      private$.state$reward <- list()
      private$.state$t <- 0L                                                    ## work this out to make work in different orders
    },
    generate_cache = function(n) {
      if (!self$bandit$has_cache) {
          self$bandit$generate_cache(n)
      }
    },
    write_to_socket = function(text, ...) {
      msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
      cat(msg)
      write.socket(self$log_socket, msg)
    },
    bandit_get_context = function(t) {
      private$.state$context <- self$bandit$get_context(t)
      self$policy$k <- private$.state$context$k
      self$policy$d <- private$.state$context$d
      private$.state$context
    },
    policy_get_decision = function(t) {
      self$policy$set_theta(private$.theta)
      (private$.state$action <- self$policy$get_action(private$.state$context))
    },
    bandit_get_reward = function(t) {
      (private$.state$reward <- self$bandit$get_reward(private$.state$action,t))
    },
    policy_set_reward = function(t) {
      (private$.theta <- self$policy$set_reward(private$.state$reward, private$.state$context))
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
