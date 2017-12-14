#' @export
RandomPolicy <- R6::R6Class(
  "RandomPolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    initialize = function(name = "Random") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0, 'succes' = 0, 'value' = 0)
    },
    get_action = function(context) {
      self$action$choice <- sample.int(context$k, 1)
      self$action$propensity <- 1/context$k
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
