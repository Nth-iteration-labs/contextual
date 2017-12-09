#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  "ThompsonSamplingPolicy",
  inherit = Contextual,
  public = list(
    alpha = 1,
    beta = 1,
    name = "",
    action = list(),
    initialize = function(alpha = 1,
                          beta =  1,
                          name = "Thompson Sampling") {
      self$alpha  <- alpha
      self$beta   <- beta
      self$name   <- name
      self$action <- list()
    },
    get_action = function(agent, context) {
      mu <- rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        mu[arm] <-  rbeta(
          1,
          self$alpha + agent$memory$theta[[arm]]$succes,
          self$beta + agent$memory$theta[[arm]]$chosen - agent$memory$theta[[arm]]$succes
        )
      }
      self$action$choice <- self$argmax(mu)
      self$action
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
#' \code{$new()} starts a new ThompsonSamplingPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name ThompsonSamplingPolicy
#' @examples
#'\dontrun{}
#'
NULL
