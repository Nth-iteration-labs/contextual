library(R6)
#' @export
RandomPolicy <- R6Class(
  "RandomPolicy",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    name = "",
    initialize = function(name = "Random") {
      self$name = name
    },
    get_action = function(agent, context) {
      return(sample.int(agent$bandit$k, 1))
    }
  )
)

#' External RandomPolicy
#'
#' RandomPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- RandomPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{RandomPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new RandomPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name RandomPolicy
#' @examples
#'\dontrun{}
#'
NULL
