library(R6)
#' @export
OraclePolicy <- R6Class(
  "OraclePolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    name = "",
    initialize = function(name = "Oracle" ) {
      self$name = name
    },
    get.action = function(agent,context){
      return(index.of.max(context$oracle))
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


