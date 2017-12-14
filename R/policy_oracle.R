#' @export
OraclePolicy <- R6::R6Class(
  "OraclePolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    initialize = function(name = "Oracle") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0, 'succes' = 0, 'value' = 0)
    },
    get_action = function(context) {
      self$action$choice <- self$argmax(context$oracle)
      self$action$propensity <- 1
      self$action
    },
    set_reward = function(reward, context) {
      self$theta[[reward$choice]]$chosen <- self$theta[[reward$choice]]$chosen + 1
      if (reward$reward == 1)
        self$theta[[reward$choice]]$succes <- self$theta[[reward$choice]]$succes + 1
      self$theta[[reward$choice]]$value <- self$theta[[reward$choice]]$value +
        (1 / self$theta[[reward$choice]]$chosen) *
        (reward$reward - self$theta[[reward$choice]]$value)
      self$theta
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
