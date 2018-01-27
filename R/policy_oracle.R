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
      self$parameters <- list('chosen' = 0, 'mu_hat' = 0)
    },
    get_action = function(context) {
      self$action$choice <- self$argmax(context$O)
      self$action$propensity <- 1
      self$action$optimal_choice <- self$argmax(context$O)                      ### repeats itself everywhere, so in superclass!
      self$action
    },
    set_reward = function(reward, context) {

      self$theta[[reward$choice]]$chosen <-
        self$theta[[reward$choice]]$chosen + 1

      self$theta[[reward$choice]]$mu_hat <-
        self$theta[[reward$choice]]$mu_hat +
        (1 / self$theta[[reward$choice]]$chosen) *
        (reward$reward - self$theta[[reward$choice]]$mu_hat)

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
