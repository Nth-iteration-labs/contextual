library(R6)
#' @export
BasicAgent <- R6Class(
  "BasicAgent",
  portable = FALSE, class = FALSE, cloneable = TRUE,
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(
      policy,
      bandit
    ) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    get.memory = function() {
      return(private$memory)
    },
    reset = function() {
      private$memory$theta =         rep(0, self$bandit$k)          # theta per arm
      private$memory$choice.counts = rep(0, self$bandit$k)          # per arm count
      private$memory$succes.counts = rep(0, self$bandit$k)          # per arm succesful count
    },
    get.action = function(context) {
      return(self$policy$get.action(self,context))
    },
    set.reward = function(reward, context = NULL) {
      inc(private$memory$choice.counts[reward$current.choice]) <- 1
      if (reward$reward == 1) inc(private$memory$succes.counts[reward$current.choice]) <- 1
      inc(private$memory$theta[reward$current.choice]) <-
        (1 / private$memory$choice.counts[reward$current.choice]) * (reward$reward - private$memory$theta[reward$current.choice])
    }
  ),
  private = list(
    memory = NULL
  )
)

#' External BasicAgent
#'
#' BasicAgent intro
#'
#' @section Usage:
#' \preformatted{b <- BasicAgent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{BasicAgent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new BasicAgent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name BasicAgent
#' @examples
#'\dontrun{}
#'
NULL
