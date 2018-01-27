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
      self$parameters <- list('chosen' = 0, 'mu_hat' = 0)
    },
    get_action = function(context) {
      self$action$choice <- sample.int(context$k, 1, replace = TRUE)
      self$action$propensity <- 1/context$k
      self$action$optimal_choice <- self$argmax(context$O)
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
