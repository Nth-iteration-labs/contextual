#' @export
AbstractPolicy <- R6::R6Class(
  "AbstractPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  public = list(
    name = "",
    action = NULL,
    theta = NULL,
    parameters = NULL,
    k = NULL,
    d = NULL,
    initialize = function(name = "ImplementMe") {
      self$name   <- name
      self$action <- list()
    },
    get_action = function(context, theta) {
      self$theta = theta
      warning("Don't forget to implement get_action()!")
      self$action
    },
    set_reward = function(reward, context) {
      warning("Don't forget to implement  set_reward()!")
      self$theta
    },
    set_parameters = function() {
      self$parameters <- list('value' = 0)
    },
    initialize_theta = function() {
      theta = list()                                                            ## if we check whats there and add defaults, becomes.. dynamic :D
      for (arm in 1:self$k) theta[[arm]] <- self$parameters
      theta
    },
    set_theta = function(theta) {
      self$theta = theta
    }
  )
)


























#' External AbstractPolicy
#'
#' AbstractPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- AbstractPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{AbstractPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new AbstractPolicy,
#' it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name AbstractPolicy
#' @examples
#'\dontrun{}
#'
NULL
