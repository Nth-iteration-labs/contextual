#' @export
EpsilonGreedyPolicy <- R6::R6Class(
  "EpsilonGreedyPolicy",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    epsilon = 0.1,
    name = "",
    action = list(),
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      self$epsilon = epsilon
      self$name = name
      self$action = list()
    },
    get_action = function(agent, context) {
      self$action = list()
      if (runif(1) < self$epsilon) {

        self$action$current_choice  = sample.int(agent$bandit$k, 1)             ### meanList.random()
        self$action$propensity      = epsilon*(1/length(agent$bandit$k))
        self$action
      } else {
        self$action$current_choice  = index_of_max(agent$get_memory()$theta)    ### meanList.max() --> here is mean of list, explicit in theta?
        self$action$propensity      = 1 - epsilon
        self$action
      }
    }
  )
)

#"simulate": "success",
#"theta": {
#  "treatment:treatment": {
#    "m": "5.863945505387279",
#    "n": "99"
#  },
#  "treatment:control": {
#    "m": "4.825426929553009",
##    "n": "3"
#  }
#},
#"experiment": "3ea45886b5"
#base.List(self.get_theta(key="treatment"), base.Mean, ["control", "treatment"]
### differences:
### - names k arms --nonsene here?
### - in memory, name key,
###    and set m and n


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
