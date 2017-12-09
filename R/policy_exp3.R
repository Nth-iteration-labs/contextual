#' @export
Exp3Policy <- R6::R6Class(
  "Exp3Policy",
  inherit = Contextual,
  public = list(
    gamma = 0.1,
    name = "",
    action = NULL,
    initialize = function(gamma =  0.1, name = "Exp3") {
      self$gamma <- gamma
      self$name  <- name
      self$action <- list()
    },
    categorical_draw = function(probs) {
      z <- runif(1)
      cum_prob <- 0.0
      lp <- length(probs)
      for (i in 1:lp) {
        cum_prob <- cum_prob + probs[i]
        if (cum_prob > z) return(i)
      }
      return(sample(1:lp, 1))
    },
    get_action = function(agent, context) {
      probs <- rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        probs[arm] <- (1 - self$gamma) *
          (agent$memory$theta[[arm]]$value /
             self$sumval(agent$memory$theta, "value"))
      }
      probs[arm] <- probs[arm] + ((self$gamma) * (1.0 / agent$bandit$k))
      self$action$choice  <- self$categorical_draw(probs)
      self$action
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
