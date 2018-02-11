#' @export
ThompsonSamplingPolicy <- R6::R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE,
  class = FALSE,
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
      self$parameters <- list('n' = 0, 'p' = 0.0, 'succes' = 0, 'mean' = 0.0)
    },
    get_action = function(context) {
      for (arm in 1:context$k) {
        theta[[arm]]$mean <-  rbeta(
          1, alpha + theta[[arm]]$succes, beta + theta[[arm]]$n - theta[[arm]]$succes
        )
      }
      action$choice <- argmaxlist(theta, "mean")
      action
    },
    set_reward = function(reward, context) {
      arm    <- reward$choice
      reward <- reward$reward
      inc(theta[[arm]]$n) <- 1
      if (reward == 1) inc(theta[[arm]]$succes) <- 1
      inc(theta[[arm]]$p) <- (reward - theta[[arm]]$p) / theta[[arm]]$n
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
