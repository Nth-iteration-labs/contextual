#' @export
EpsilonFirstPolicy <- R6::R6Class(
  "EpsilonFirstPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    first = NULL,
    initialize = function(first = 100, name = "EpsilonFirst") {
      super$initialize(name)
      self$first <- first
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      if (sum_of_param(theta, "n") < first) {
        action$choice       <- sample.int(context$k, 1, replace = TRUE)
        action$propensity   <- (1/context$k)
      } else {
        action$choice       <- max_in_param(theta, "mean", equal_is_random = FALSE)
        action$propensity   <- 1
      }
      action
    },
    set_reward = function(reward, context) {
      arm      <- reward$choice
      reward   <- reward$reward

      inc(theta[[arm]]$n) <- 1
      if (sumval(theta, "n") < first - 1)
        inc(theta[[arm]]$mean ) <- (reward - theta[[arm]]$mean) / theta[[arm]]$n

      theta
    }
  )
)

#' External EpsilonFirstPolicy
#'
#' EpsilonFirstPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- EpsilonFirstPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{EpsilonFirstPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new EpsilonFirstPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name EpsilonFirstPolicy
#' @examples
#'\dontrun{}
#'
NULL
