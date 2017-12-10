#' @export
Exp3Agent <- R6::R6Class(
  "Exp3Agent",
  portable = FALSE,
  inherit = AbstractAgent,
  private = list(
    initiate_theta = function(t=NA) {
      theta_arm <- list('value' = 1)
      for (arm in 1:self$bandit$k) private$.theta[[arm]] <- theta_arm
    }
  )
)

#' External Exp3Agent
#'
#' Exp3Agent intro
#'
#' @section Usage:
#' \preformatted{b <- Exp3Agent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Exp3Agent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Exp3Agent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Exp3Agent
#' @examples
#'\dontrun{}
#'
NULL
