#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    epsilon = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      super$initialize(name)
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      if (runif(1) < epsilon) {
        action$choice       <- sample.int(context$k, 1, replace = TRUE)
        action$propensity   <- epsilon*(1/context$k)
      } else {
        action$choice       <- max_in_param(theta, "mean")
        action$propensity   <- 1 - epsilon
      }
      action
    },
    set_reward = function(reward, context) {
      arm <- reward$choice ; reward <- reward$reward
      inc(theta[[arm]]$n)    <- 1
      inc(theta[[arm]]$mean) <- (reward - theta[[arm]]$mean) / theta[[arm]]$n
      theta
    }
  )
)

#' External EpsilonGreedyPolicy
#'
#' EpsilonGreedyPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- EpsilonGreedyPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{EpsilonGreedyPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new EpsilonGreedyPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name EpsilonGreedyPolicy
#' @examples
#'\dontrun{}
#'
NULL
