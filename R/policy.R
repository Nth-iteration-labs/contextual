#' @export
Policy <- R6::R6Class(
  "Policy",
  portable = FALSE,
  class = FALSE,
  public = list(
    name          = "",
    action        = NULL,
    theta         = NULL,
    theta_to_arms = NULL,
    k             = NULL,
    d             = NULL,
    initialize = function(name = "Not implemented") {
      self$theta <- list()
      self$name   <- name
      self$action <- list()
    },
    get_action = function(context, theta, t) {
      # Selects an arm based on self$theta and context, returns it in action$choice.
      stop("Policy$get_action() has not been implemented.", call. = FALSE)
    },
    set_reward = function(context, action, reward, t) {
      # Updates parameters in theta based on reward awarded by bandit.
      stop("Policy$set_reward() has not been implemented.", call. = FALSE)
    },
    set_parameters = function() {
      # Policy parameter (not theta!) initialisation happens here.
      stop("Policy$set_parameters() has not been implemented.", call. = FALSE)
    },
    initialize_theta = function() {
      # Called during contextual's initialisation.
      # Copies theta_to_arms k times, makes the copies available through theta.
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
#' @name Policy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- Policy()
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{name}}{
#'    character string specifying this policy. \code{name}
#'    is, amongst others, saved to the History log and displayed in summaries and plots.
#'   }
#' }
#'
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(alpha = 1, name = "LinUCB")}}{ Generates a new \code{LinUCBDisjointPolicy} object. Arguments are defined in the Argument section above.}
#' }
#'
#' \describe{
#'   \item{\code{set_parameters()}}{each policy needs to assign the parameters it wants to keep track of
#'   to list \code{self$theta_to_arms} that has to be defined in \code{set_parameters()}'s body.
#'   The parameters defined here can later be accessed by arm index in the following way:
#'   \code{theta[[index_of_arm]]$parameter_name}
#'   }
#' }
#'
#' \describe{
#'   \item{\code{get_action(context)}}{
#'     here, a policy decides which arm to choose, based on the current values
#'     of its parameters and, potentially, the current context.
#'    }
#'   }
#'
#'  \describe{
#'   \item{\code{set_reward(reward, context)}}{
#'     in \code{set_reward(reward, context)}, a policy updates its parameter values
#'     based on the reward received, and, potentially, the current context.
#'    }
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{SyntheticBandit}}
#'
#'
#'
NULL
