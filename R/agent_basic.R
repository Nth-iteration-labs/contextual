#' @export
BasicAgent <- R6::R6Class(
  "BasicAgent",
  portable = FALSE,
  private = list(
    initiate_theta = function(t=NA) {
      theta_arm <- list('chosen' = 0,'succesful' = 0, 'value' = 0, 'mu' = 0.0, 'alpha' = 1.0, 'beta' = 1.0)
      for (arm in 1:self$bandit$k) private$.theta[[arm]] <- theta_arm
    }
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
