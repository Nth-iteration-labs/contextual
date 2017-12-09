#' @import R6
#' @export
Contextual <- R6::R6Class(
  "Contextual",
  public = list(
    argmax = function(x, list_element_name = NA)
    {
      if (!is.na(list_element_name)) {
        x <- sapply(x, `[[`, list_element_name)
      }
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L) {
        return(sample(y, 1L))
      } else {
        return(y)
      }
    },
    sumval = function(x, list_element_name)
    {
      stopifnot(is.list(x), length(x) > 2)
      stopifnot(is.character(list_element_name), length(list_element_name) > 0)
      sum(sapply(x, `[[`, as.character(list_element_name)))
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
