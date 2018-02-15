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
    initialize = function(name = "Not implemented") {
      self$name   <- name
      self$action <- list()
    },
    get_action = function(context, theta) {
      self$theta = theta
      stop("AbstractPolicy$get_action() has not been implemented",
           call. = FALSE)
    },
    set_reward = function(reward, context) {
      stop("AbstractPolicy$set_reward() has not been implemented",
           call. = FALSE)
    },
    set_parameters = function() {
      stop("AbstractPolicy$set_parameters() has not been implemented",
           call. = FALSE)
    },
    initialize_theta = function() {
      theta <- list()
      for (param_index in 1L:length(parameters)) {
        theta[[ names(self$parameters)[param_index] ]] <- rep(list(self$parameters[[param_index]]),self$k)
      }
      theta
    },
    set_theta = function(theta) {
      self$theta <- theta
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
