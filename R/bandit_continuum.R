#' @export
ContinuumBandit <- R6::R6Class(
  "ContinuumBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    arm_function = NULL,
    initialize   = function(FUN) {
      super$initialize()
      self$arm_function <- FUN
      self$d            <- 1
      self$k            <- 1
    },
    get_context = function(t) {
      contextlist       <- list()
      contextlist$k     <- self$k
      contextlist$d     <- self$d
      contextlist
    },
    do_action = function(context, action, t) {
      rewardlist        <- list()
      rewardlist$reward <- self$arm_function(action$choice)
      rewardlist
    }
  )
)

#' Bandit: Continuum
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
#' @importFrom R6 R6Class
#' @name ContinuumBandit
#' @examples
#'\dontrun{}
#'
NULL

