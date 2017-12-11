#' @export
OraclePolicy <- R6::R6Class(
  "OraclePolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    name = "",
    action = NULL,
    initialize = function(name = "Oracle") {
      self$name <- name
      self$action <- list()
    },
    set_theta = function(arms, features) {
      parameters_per_arm <- list('chosen' = 0, 'succes' = 0, 'value' = 0)
      populate_theta(arms, parameters_per_arm)
    },
    get_action = function(context, theta) {
      self$action$choice  <- self$argmax(context$oracle)
      self$action$propensity <- 1
      self$action$theta  <- theta
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
