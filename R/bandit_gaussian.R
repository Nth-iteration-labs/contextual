library(R6)

#' GaussianBandit
#'
#' Bandit of type: Basic Gaussian
#'
#' @section Usage:
#' \preformatted{p <- process$new(command = NULL, args, commandline = NULL,
#'                  stdout = TRUE, stderr = TRUE)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{k}{An integer: how many arms.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new process, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' \code{$reset()} Resets the bandit.
#'
#' \code{$pull(action)} Returns reward and True if action is optimal.
#'
#' @importFrom R6 R6Class
#' @name GaussianBandit
#' @examples
#'
NULL

#' @export
GaussianBandit <- R6Class(

  "GaussianBandit",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    k = NULL,
    mu = NULL,
    sigma = NULL,
    action_values = NULL,
    optimal = NULL,

    initialize = function(k = NA, mu=0, sigma= 1) {
      self$k = k                                                   # arms
      self$mu = mu                                                 # mean
      self$sigma = sigma                                           # standard deviation
      self$reset()                                                 # reset
    },
    reset = function() {
      self$action_values = rnorm(self$k, self$mu, self$sigma )     # result action values
      self$optimal = index_of_max(self$action_values)              # optimal result
    },
    get_reward = function(action) {
      reward = setNames(
          list( rnorm(1, self$action_values[action]) , action == self$optimal ),
          c("reward", "is_optimal")
      )
      return(reward)
    }

  )
)
