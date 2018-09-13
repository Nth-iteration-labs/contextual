#' @export
ContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    arm_function = NULL,
    class_name = "ContinuumBandit",
    initialize   = function(FUN) {
      self$arm_function <- FUN
      super$initialize()
      self$d            <- 1
      self$k            <- 1
    },
    get_context = function(t) {
      context           <- list()
      context$k         <- self$k
      context$d         <- self$d
      context
    },
    get_reward = function(t, context, action) {
      reward            <- list()
      reward$reward     <- self$arm_function(action$choice)
      reward
    }
  )
)

#' Bandit: ContinuumBandit
#'
#' ContinuumBandit intro
#'
#' @section Usage:
#' \preformatted{b <- ContinuumBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @name ContinuumBandit
#' @family contextual subclasses
#'
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{ContinuumBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new ContinuumBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
NULL

