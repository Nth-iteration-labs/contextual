#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE,
  inherit = AbstractPolicy,
  public = list(
    alpha = 1,
    beta = 1,
    initialize = function(alpha = 1, beta =  1, name = "Thompson Sampling") {
      super$initialize(name)
      self$alpha  <- alpha
      self$beta   <- beta
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0,
                              'probability' = 0.0,
                              'succes' = 0,
                              'mu' = 0.0)
    },
    get_action = function(context) {
      for (arm in 1:context$k) {
        self$theta[[arm]]$mu <-  rbeta(
          1,
          self$alpha + self$theta[[arm]]$succes,
          self$beta  + self$theta[[arm]]$chosen - self$theta[[arm]]$succes
        )
      }
      self$action$choice <- self$argmax(self$theta,"mu")
      self$action
    },
    set_reward = function(reward, context) {
      self$theta[[reward$choice]]$chosen <- self$theta[[reward$choice]]$chosen + 1
      if (reward$reward == 1)
        self$theta[[reward$choice]]$succes <- self$theta[[reward$choice]]$succes + 1
      self$theta[[reward$choice]]$probability <- self$theta[[reward$choice]]$probability +
        (1 / self$theta[[reward$choice]]$chosen) *
        (reward$reward - self$theta[[reward$choice]]$probability)
      self$theta
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
