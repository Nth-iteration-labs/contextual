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
      stop("AbstractPolicy$get_action() has not been implemented",
           call. = FALSE)
    },
    set_reward = function(context, action, reward) {
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


#' Policy: Abstract Root Class
#'
#' The R6 class \code{Policy} is the root of all policies implemented by \code{\{contextual\}}.
#' That is, every \code{Policy} class in the \code{\{contextual\}} package has to inherit from, and implement the methods of,
#' the \code{Policy} superclass.
#'
#' @name AbstractPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- Policy()
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{AbstractBandit}}, \code{\link{BasicBandit}},
#' \code{\link{OfflineLiBandit}}, \code{\link{SyntheticBandit}}
#'
#'
#'
NULL
