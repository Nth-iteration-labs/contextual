#' @export
LinUCBAgent <- R6::R6Class(
  "LinUCBAgent",
  portable = FALSE,
  private = list(
    initiate_theta = function(t=NA) {
      theta_arm <- list(
        'A' = diag(1,self$bandit$d),                                            # A is a d*d identity matrix
        'b' = rep(0,self$bandit$d)                                              # b is a 0 vector of length
      )
      for (arm in 1:self$bandit$k) private$.theta[[arm]] <- theta_arm
    }
  )
)

#' External LinUCBAgent
#'
#' LinUCBAgent intro
#'
#' @section Usage:
#' \preformatted{b <- LinUCBAgent$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{LinUCBAgent} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new LinUCBAgent, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name LinUCBAgent
#' @examples
#'\dontrun{}
#'
NULL

