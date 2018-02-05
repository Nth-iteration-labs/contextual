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
      for (arm in 1:context$k) {
         probs[arm] <- (1 - self$gamma) * (self$theta[[arm]]$weight / self$sumval(self$theta, "weight"))
      }
      probs[arm] <- probs[arm] + ((self$gamma) * (1.0 / context$k))
      self$action$choice  <- self$categorical_draw(probs)
      self$action$optimal_choice <- self$argmax(context$O)
      self$action
    },
    set_reward = function(reward, context) {
      probs <- rep(0.0, context$k)
      for (arm in 1:context$k) {
         probs[arm] <- (1 - self$gamma) * (self$theta[[arm]]$weight / self$sumval(self$theta, "weight"))
         probs[arm] <-  probs[arm] + (self$gamma) * (1.0 / context$k)
      }
      x <- reward$reward /  probs[reward$choice]
      growth_factor <- exp((self$gamma / context$k) * x)
      self$theta[[reward$choice]]$weight <- self$theta[[reward$choice]]$weight * growth_factor
      self$theta
    },
    categorical_draw = function(probs) {
      arms = length(probs)
      cummulative_probability <- 0.0
      for (arm in 1:arms) {
        cummulative_probability <-  cummulative_probability + probs[arm]
        if ( cummulative_probability > runif(1) ) return(arm)
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
