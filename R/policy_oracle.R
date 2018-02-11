#' @export
OraclePolicy <- R6::R6Class(
  "OraclePolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    initialize = function(name = "Oracle") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      action$propensity <- 1
      action$choice <- argmax(context$O)
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward
      inc(theta[[arm]]$n) <- 1
      inc(theta[[arm]]$mean) <- (reward - theta[[arm]]$mean) / theta[[arm]]$n
      theta
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
