#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    epsilon = 0.1,
    name = "",
    action = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      self$epsilon <- epsilon
      self$name <- name
      self$action <- list()
    },
    get_action = function(context, theta) {
      if (runif(1) < self$epsilon) {
        self$action$choice  <- sample.int(context$k, 1)
        self$action$propensity <- self$epsilon*(1/context$k)
      } else {
        self$action$choice <- self$argmax(theta,"value")
        self$action$propensity <- 1 - self$epsilon
      }
      self$action
    },
    set_reward = function(reward, context, theta) {

      theta[[reward$choice]]$chosen <- theta[[reward$choice]]$chosen + 1

      if (reward$reward == 1)
        theta[[reward$choice]]$succes <- theta[[reward$choice]]$succes + 1

      theta[[reward$choice]]$value <- theta[[reward$choice]]$value +
        (1 / theta[[reward$choice]]$chosen) *
        (reward$reward - theta[[reward$choice]]$value)

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
