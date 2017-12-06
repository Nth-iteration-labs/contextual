library(R6)
#' @export
EpsilonGreedyPolicy <- R6Class(
  "EpsilonGreedyPolicy",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    epsilon = 0.1,
    name = "",
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      self$epsilon = epsilon
      self$name = name
    },
    get_action = function(agent, context) {
      if (runif(1) < self$epsilon) {
        return(sample.int(agent$bandit$k, 1))
      } else {
        return(index_of_max(agent$get_memory()$theta))
      }
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
