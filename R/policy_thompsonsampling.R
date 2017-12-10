#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = 1,
    beta = 1,
    name = "",
    action = NULL,
    initialize = function(alpha = 1,
                          beta =  1,
                          name = "Thompson Sampling") {
      self$alpha  <- alpha
      self$beta   <- beta
      self$name   <- name
      self$action <- list()
    },
    get_action = function(context, theta) {
      mu <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        mu[arm] <-  rbeta(
          1,
          self$alpha + theta[[arm]]$succes,
          self$beta  + theta[[arm]]$chosen - theta[[arm]]$succes
        )
      }
      self$action$mu <- mu
      self$action$choice <- self$argmax(mu)
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

#' External ThompsonSamplingPolicy
#'
#' ThompsonSamplingPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- ThompsonSamplingPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{ThompsonSamplingPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new ThompsonSamplingPolicy,
#' it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name ThompsonSamplingPolicy
#' @examples
#'\dontrun{}
#'
NULL
