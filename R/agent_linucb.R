#' @export
LinUCBAgent <- R6::R6Class(
  "LinUCBAgent",
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
      private$.memory$theta <- list()                                           # instantiate memory for theta
      theta.arm <- list(
        'A' = diag(1,bandit$d),                                                 # A is a d*d identity matrix
        'b' = rep(0,bandit$d)                                                   # b is a 0 vector of length
      )
      for (i in 1:self$bandit$k) {
        private$.memory$theta[[i]] <- theta.arm                                 # assign per arm to theta in memory
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
    set_reward = function(reward, context) {
      X <- as.vector(context$X)
      private$.memory$theta[[reward$choice]]$A <- private$.memory$theta[[reward$choice]]$A + outer(X, X)
      private$.memory$theta[[reward$choice]]$b <- private$.memory$theta[[reward$choice]]$b + reward$reward * X
    }
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

