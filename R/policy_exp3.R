#' @export
Exp3Policy <- R6::R6Class(
  "Exp3Policy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    gamma = NULL,
    initialize = function(gamma =  0.1, name = "Exp3") {
      super$initialize(name)
      self$gamma <- gamma
    },
    set_parameters = function() {
      self$parameters <- list('weight' = 1)
    },
    get_action = function(context) {
      probs <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (theta[[i]]$weight / sum_of_param(theta, "weight"))
      }
      inc(probs[i])  <- ((gamma) * (1.0 / context$k))
      action$choice  <- categorical_draw(probs)
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward
      probs  <- rep(0.0, context$k)
      for (i in 1:context$k) {
         probs[i] <- (1 - gamma) * (theta[[i]]$weight / sum_of_param(theta, "weight"))
         inc(probs[i]) <- gamma  / context$k
      }
      growth_factor <- exp((gamma / context$k) * reward / probs[arm])
      theta[[arm]]$weight <- theta[[arm]]$weight * growth_factor
      theta
    },
    categorical_draw = function(probs) {
      arms = length(probs)
      cumulative_probability <- 0.0
      for (i in 1:arms) {
        inc(cumulative_probability) <- probs[i]
        if ( cumulative_probability > runif(1) ) return(i)
      }
      sample(arms, 1, replace = TRUE)
    }
  )
)


















#' External Exp3Policy
#'
#' Exp3Policy intro
#'
#' @section Usage:
#' \preformatted{b <- Exp3Policy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Exp3Policy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Exp3Policy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Exp3Policy
#' @examples
#'\dontrun{}
#'
NULL
