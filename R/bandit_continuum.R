#' @export
ContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    arm_function = NULL,
    class_name = "ContinuumBandit",
    initialize   = function(arm_function) {
      self$arm_function <- arm_function
      super$initialize()
      self$d            <- 1
      self$k            <- 1
    },
    get_context = function(t) {
      contextlist       <- list()
      contextlist$k     <- self$k
      contextlist$d     <- self$d
      contextlist
    },
    get_reward = function(t, context, action) {
      rewardlist        <- list()
      rewardlist$reward <- self$arm_function(action$choice)
      rewardlist
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

