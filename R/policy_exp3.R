#' @export
Exp3Policy <- R6::R6Class(
  "Exp3Policy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    gamma = 0.1,
    initialize = function(gamma =  0.1, name = "Exp3") {
      super$initialize(name)
      self$gamma <- gamma
    },
    set_parameters = function() {
      self$parameters <- list('value' = 1)
    },
    get_action = function(context) {
      probabilities <- rep(0.0, context$k)
      for (arm in 1:context$k) {
         probabilities[arm] <-
           (1 - self$gamma) * (self$theta[[arm]]$value / self$sumval(self$theta, "value"))
      }
       probabilities[arm] <- probabilities[arm] + ((self$gamma) * (1.0 / context$k))
      self$action$choice  <- self$categorical_draw(probabilities)
      self$action
    },
    set_reward = function(reward, context) {
      probabilities <- rep(0.0, context$k)
      for (arm in 1:context$k) {
         probabilities[arm] <- (1 - self$gamma) * (self$theta[[arm]]$value / self$sumval(self$theta, "value"))
         probabilities[arm] <-  probabilities[arm] + (self$gamma) * (1.0 / context$k)
      }
      x <- reward$reward /  probabilities[reward$choice]
      growth_factor <- exp((self$gamma / context$k) * x)
      self$theta[[reward$choice]]$value <-
        self$theta[[reward$choice]]$value * growth_factor
      self$theta
    },
    categorical_draw = function(probabilities) {
      arms = length(probabilities)
      cummulative_probability <- 0.0
      for (arm in 1:arms) {
         cummulative_probability <-  cummulative_probability + probabilities[arm]
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
