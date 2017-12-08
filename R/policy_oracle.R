#' @export
OraclePolicy <- R6::R6Class(
  "OraclePolicy",
  inherit = Contextual,
  public = list(
    name = "",
    action = list(),
    initialize = function(name = "Oracle") {
      self$name = name
      self$action = list()
    },
    get_action = function(agent, context) {
      self$action$current_choice  = self$index_of_max(context$oracle)
      self$action$propensity      = 1
      self$action
    }
  )
)

#' External OraclePolicy
#'
#' OraclePolicy intro
#'
#' @section Usage:
#' \preformatted{b <- OraclePolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{OraclePolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new OraclePolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name OraclePolicy
#' @examples
#'\dontrun{}
#'
NULL
