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
    set_theta = function(arms, features) {
      parameters_per_arm <- list('chosen' = 0,
                                 'probability' = 0.0,
                                 'succes' = 0,
                                 'mu' = 0.0)
      populate_theta(arms, parameters_per_arm)
    },
    get_action = function(context, theta) {
      for (arm in 1:context$k) {
        theta[[arm]]$mu <-  rbeta(
          1,
          self$alpha + theta[[arm]]$succes,
          self$beta  + theta[[arm]]$chosen - theta[[arm]]$succes
        )
      }
      self$action$choice <- self$argmax(theta,"mu")
      self$action$theta  <- theta                                               ## really just to save, so is this smart?
      self$action
    },
    set_reward = function(reward, context, theta) {

      theta[[reward$choice]]$chosen <- theta[[reward$choice]]$chosen + 1
      if (reward$reward == 1)
        theta[[reward$choice]]$succes <- theta[[reward$choice]]$succes + 1
      theta[[reward$choice]]$probability <- theta[[reward$choice]]$probability +
        (1 / theta[[reward$choice]]$chosen) *
        (reward$reward - theta[[reward$choice]]$probability)
      #print(as.data.frame(theta))
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
