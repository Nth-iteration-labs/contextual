library(R6)

#' @export
Contextual <- R6Class(
  "Contextual",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    index_of_max = function(x)
    {
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L) {
        return(sample(y, 1L))
      } else {
        return(y)
      }
    },
    "inc<-" = function(x, value) {
      x + value
    },
    "dec<-" = function(x, value) {
      x - value
    },
    "mult<-" = function(x, value) {
      x * value
    }
  )
)

#' External Contextual
#'
#' Contextual intro
#'
#' @section Usage:
#' \preformatted{b <- Contextual$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Contextual} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Contextual, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Contextual
#' @examples
#'\dontrun{}
#'
NULL
