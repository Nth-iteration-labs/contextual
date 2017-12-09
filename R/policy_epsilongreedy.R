#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  inherit = Contextual,
  public = list(
    epsilon = 0.1,
    name = "",
    action = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      self$epsilon <- epsilon
      self$name <- name
      self$action <- list()
    },
    get_action = function(agent, context) {
      if (runif(1) < self$epsilon) {
        self$action$choice  <- sample.int(agent$bandit$k, 1)
        self$action$propensity <- self$epsilon*(1/length(agent$bandit$k))
      } else {
        self$action$choice <- self$argmax(agent$memory$theta,"value")
        self$action$propensity <- 1 - self$epsilon
      }
      self$action
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
