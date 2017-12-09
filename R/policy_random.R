#' @export
RandomPolicy <- R6::R6Class(
  "RandomPolicy",
  public = list(
    name = "",
    action = NULL,
    initialize = function(name = "Random") {
      self$name <- name
      self$action <- list()
    },
    get_action = function(agent, context) {
      self$action$choice <- sample.int(agent$bandit$k, 1)
      self$action$propensity <- 1/agent$bandit$k
      self$action
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
