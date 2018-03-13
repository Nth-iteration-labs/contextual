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
    theta_to_arms = NULL,
    k             = NULL, # n of arms
    d             = NULL, # n of context features
    d_context           = NULL, # subset n of CONTEXT FEATURES
    d_arms           = NULL, # subset n of ARM FEATURES
    initialize = function(name = "Not implemented") {
      self$theta <- list()
      self$name   <- name
      self$action <- list()
    },
    get_action = function(context, theta, t) {
      stop("AbstractPolicy$get_action() has not been implemented",
           call. = FALSE)
    },
    set_reward = function(context, action, reward, t) {
      stop("AbstractPolicy$set_reward() has not been implemented",
           call. = FALSE)
    },
    set_parameters = function() {
      stop("AbstractPolicy$set_parameters() has not been implemented",
           call. = FALSE)
    },
    initialize_theta = function() {
      if (!is.null(theta_to_arms)) {
        for (param_index in 1L:length(theta_to_arms)) {
          theta[[ names(self$theta_to_arms)[param_index] ]] <- rep(list(self$theta_to_arms[[param_index]]),self$k)
        }
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
