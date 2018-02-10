#' @export
EpsilonFirstPolicy <- R6::R6Class(
  "EpsilonFirstPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    n_first = NULL,
    initialize = function(n_first = 100, name = "EpsilonFirst") {
      super$initialize(name)
      self$n_first <- n_first
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0, 'succes' = 0, 'mu_hat' = 0)
    },
    get_action = function(context) {
      print(self$sumval(self$theta, "chosen"))
      if (self$sumval(self$theta, "chosen") < self$n_first) {
        self$action$choice  <- sample.int(context$k, 1, replace = TRUE)
        self$action$propensity <- (1/context$k)
      } else {
        print(paste0("    " ,self$argmaxlist(self$theta,"mu_hat", equal_is_random = F)))
        self$action$choice <- self$argmaxlist(self$theta,"mu_hat", equal_is_random = FALSE)
        self$action$propensity <- 1
      }
      self$action$optimal_choice <- self$argmax(context$O)
      self$action
    },
    set_reward = function(reward, context) {
      self$theta[[reward$choice]]$chosen  <- self$theta[[reward$choice]]$chosen + 1
      if (self$sumval(self$theta, "chosen") < self$n_first - 1)                              ########### +1
        self$theta[[reward$choice]]$mu_hat  <- self$theta[[reward$choice]]$mu_hat +
          (1 / self$theta[[reward$choice]]$chosen) * (reward$reward - self$theta[[reward$choice]]$mu_hat)
      self$theta
    }
  )
)

#' External EpsilonFirstPolicy
#'
#' EpsilonFirstPolicy intro
#'
#' @section Usage:
#' \preformatted{b <- EpsilonFirstPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{EpsilonFirstPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new EpsilonFirstPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name EpsilonFirstPolicy
#' @examples
#'\dontrun{}
#'
NULL
