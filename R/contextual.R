#' @import R6
#' @export
Contextual <- R6::R6Class(
  "Contextual",
  portable = FALSE,
  class = FALSE,
  private = list(.hash = NULL),
  active = list(
    hash = function(value) {
      if (missing(value)) {
        private$.hash
      }
    }
  ),
  public = list(
    initialize = function() {
      private$.hash = sub('<environment: (.*)>', '\\1',  capture.output(self))

    },
    argmaxlist = function(x, list_element_name = NA)
    {
      x <- lname_to_vector(x, list_element_name)
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L)
        sample(y, 1L, replace = TRUE)
      else
        y
    },
    argmax = function(x)
    {
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L)
        sample(y, 1L, replace = TRUE)
      else
        y
    },
    sumval = function(x, list_element_name)
    {
      sum(lname_to_vector(x, list_element_name))
    },
    lname_to_vector = function(x, list_element_name)
    {
      unlist(lapply(x, `[[`, list_element_name) , FALSE, FALSE)
    },
    repmat = function(X, m, n) {
      mx = dim(X)[1]
      nx = dim(X)[2]
      matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = TRUE)
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
