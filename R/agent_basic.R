#' @export
BasicAgent <- R6::R6Class(
  "BasicAgent",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,
  public = list(
    policy = NULL,
    bandit = NULL,
    initialize = function(policy,
                          bandit) {
      self$bandit = bandit
      self$policy = policy
      self$reset()
    },
    get_memory = function() {
      return(private$memory)
    },
    reset = function() {
      private$memory$theta =         rep(0, self$bandit$k)                      # theta per arm
      private$memory$choice.counts = rep(0, self$bandit$k)                      # per arm count
      private$memory$succes.counts = rep(0, self$bandit$k)                      # per arm succesful count
    },
    get_action = function(context) {
      return(self$policy$get_action(self, context))
    },
    set_reward = function(reward, context = NULL) {
      inc(private$memory$choice.counts[reward$current_choice]) <- 1
      if (reward$reward == 1)
        inc(private$memory$succes.counts[reward$current_choice]) <- 1
      inc(private$memory$theta[reward$current_choice]) <-
        (1 / private$memory$choice.counts[reward$current_choice]) *
        (reward$reward - private$memory$theta[reward$current_choice])
    }
  ),
  private = list(memory = NULL)
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
