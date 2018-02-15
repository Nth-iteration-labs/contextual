#' @export
RandomPolicy <- R6::R6Class(
  "RandomPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    initialize = function(name = "Random") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$parameters <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(context) {
      action$choice <- sample.int(context$k, 1, replace = TRUE)
      action$propensity <- 1/context$k
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward
      inc(theta$n[[arm]]) <- 1
      inc(theta$mean[[arm]]) <- (reward - theta$mean[[arm]]) / theta$n[[arm]]
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
