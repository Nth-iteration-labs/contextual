#' @export
LinUCBAgent <- R6::R6Class(
  "LinUCBAgent",
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
      private$memory$theta = list()                                             # instantiate memory for theta
      theta.arm = list(
        'A' = diag(1,bandit$d),                                                 # A is a d*d identity matrix
        'b' = rep(0,bandit$d)                                                   # b is a 0 vector of length
      )
      for (i in 1:self$bandit$k) {
        private$memory$theta[[i]] = theta.arm                                   # assign per arm to theta in memory
      }
    },
    get_action = function(context) {
      return(self$policy$get_action(self, context))
    },
    set_reward = function(reward, context) {
      X = as.vector(context$X)
      inc(private$memory$theta[[reward$current_choice]]$A) <-
        outer(X, X)
      inc(private$memory$theta[[reward$current_choice]]$b) <-
        reward$reward * X
    }
  ),
  private = list(
    memory = list()
  )
)

#' External LinUCBAgent
#'
#' LinUCBAgent intro
#'
#' @section Usage:
#' \preformatted{b <- LinUCBAgent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{LinUCBAgent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new LinUCBAgent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name LinUCBAgent
#' @examples
#'\dontrun{}
#'
NULL

