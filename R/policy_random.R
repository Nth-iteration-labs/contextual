#' @export
RandomPolicy <- R6::R6Class(
  "RandomPolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    name = "",
    action = NULL,
    initialize = function(name = "Random") {
      self$name <- name
      self$action <- list()
    },
    set_theta = function(arms, features) {
      parameters_per_arm <- list('chosen' = 0, 'succes' = 0, 'value' = 0)
      populate_theta(arms, parameters_per_arm)
    },
    get_action = function(context, theta) {
      self$action$choice <- sample.int(context$k, 1)
      self$action$propensity <- 1/context$k
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
