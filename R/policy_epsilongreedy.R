#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    epsilon = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      super$initialize(name)
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0, 'succes' = 0, 'mu_hat' = 0)
    },
    get_action = function(context) {
      if (runif(1) < self$epsilon) {
        self$action$choice  <- sample.int(context$k, 1, replace = TRUE)
        self$action$propensity <- self$epsilon*(1/context$k)
      } else {
        self$action$choice <- self$argmaxlist(self$theta,"mu_hat")
        self$action$propensity <- 1 - self$epsilon
      }
      self$action$optimal_choice <- self$argmax(context$O)
      self$action
    },
    set_reward = function(reward, context) {

      self$theta[[reward$choice]]$chosen  <-

        self$theta[[reward$choice]]$chosen + 1

      self$theta[[reward$choice]]$mu_hat  <-

        self$theta[[reward$choice]]$mu_hat +
        (1 / self$theta[[reward$choice]]$chosen) *
        (reward$reward - self$theta[[reward$choice]]$mu_hat)

      self$theta
    }
  )
)


#' External EpsilonGreedyPolicy
#'
#' EpsilonGreedyPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- EpsilonGreedyPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{EpsilonGreedyPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new EpsilonGreedyPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name EpsilonGreedyPolicy
#' @examples
#'\dontrun{}
#'
NULL
