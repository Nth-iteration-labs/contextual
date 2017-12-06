library(R6)
#' @export
ThompsonSamplingPolicy <- R6Class(
  "ThompsonSamplingPolicy",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    alpha = 1,
    beta = 1,
    name = "",
    action = list(),
    initialize = function(alpha = 1,
                          beta =  1,
                          name = "Thompson Sampling") {
      self$alpha  = alpha
      self$beta   = beta
      self$name   = name
      self$action = list()
    },
    get_action = function(agent, context) {
      mu = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        mu[arm] =  rbeta(
          1,
          self$alpha + agent$get_memory()$succes.counts[arm],
          self$beta + agent$get_memory()$choice.counts[arm] - agent$get_memory()$succes.counts[arm]
        )
      }
      self$action$current_choice  = index_of_max(mu)
      self$action$propensity      = 0 # ###################
      return(self$action)
      return()
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
