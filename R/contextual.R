#' @importFrom R6 R6Class
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
    "inc<-" = function(x, value) {
      x + value
    },
    "dec<-" = function(x, value) {
      x - value
    },
    max_in = function(x, equal_is_random = TRUE)
    {
      x <- unlist(x)
      y <- seq_along(x)[x == max(x)]
      if (length(y) > 1L)  {
        if (equal_is_random) {
          return(sample(y, 1L, replace = TRUE))
        } else {
          return(y[1])
        }
      } else {
        return(y)
      }
    },
    sum_of = function(x)
    {
      sum(unlist(x))
    }
  )
)

#' Contextual Utility and Root Class
#'
#' The R6 class \code{Contextual} is the root of the \code{\{contextual\}} package's class hierarchy.
#' That is, every class in the \code{\{contextual\}} package has \code{Contextual} as a superclass.
#' Contextual contains several utility functions that are used throughtout \code{\{contextual\}}.
#'
#' @section Usage:
#' \preformatted{
#' contextual <- Contextual$new()
#' }
#'
#' @section Package:
#'
#' The \code{\{contextual\}} package enables you to simulate and analyze
#' contextual multi-armed bandit algorithms with poise.
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{hash}}{
#'   Returns an unique hash for any instantiated \code{\{contextual\}} child object. This hash value is acquired from the R6 object's environment.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new()}}{ Generates a new \code{Contextual} object.}
#' }
#'
#' \describe{
#'   \item{\code{max_in(x, equal_is_random = TRUE))}}{
#'   Returns the index of the maximum value in list \code{x}.
#'
#'   If there is a tie and \code{equal_is_random} is \code{TRUE},
#'   the index of one of the tied maxima is returned at random.
#'
#'   If \code{equal_is_random} is \code{FALSE},
#'   the maximum with the lowest index number is returned.
#'   }
#' }
#'
#' \describe{
#'   \item{\code{sum_of(x)}}{
#'   Returns the sum of the values of the elements of a list \code{x}.
#'   }
#' }
#'
#' \describe{
#'   \item{\code{inc(x)<-value}}{
#'   Increments x by value. Equivalent to x <- x + value. (orginal: \code{Hmisc})
#'   }
#' }
#' \describe{
#'   \item{\code{dec(x)<-value}}{
#'   Decrements x by value. Equivalent to x <- x - value. (orginal: \code{Hmisc})
#'   }
#' }
#'
#' @references
#'
#' Winston Chang (2017). R6: Classes with Reference Semantics. R package version 2.2.2. https://CRAN.R-project.org/package=R6
#'
#' Frank E Harrell Jr, with contributions from Charles Dupont and many others. (2017). Hmisc:   Harrell Miscellaneous. R package version 4.0-3. https://CRAN.R-project.org/package=Hmisc
#'
#' @seealso
#'
#' Online: \href{https://nth-iteration-labs.github.io/contextual/index.html}{Documentation}
#'
#' @name Contextual
#' @family contextual classes
#' @examples
#' contextual <- Contextual$new()
#'
#' print(contextual$hash)
#'
#' theta = list(par_one = list(1,2,3), par_two = list(2,3,4))
#'
#' contextual$max_in(theta$par_one)
#'
#' contextual$sum_of(theta$par_one)
#'
#' # For this example section, the inc<- and dec<-
#' # functions need to be extracted from our
#' # contextual object.
#'
#' "inc<-" <- contextual$`inc<-`
#' "dec<-" <- contextual$`dec<-`
#'
#' # Within our contextual class hierarchy,
#' # these functions do not need to be
#' # extracted and can be applied directly
#' # as illustrated below.
#'
#' x <- 5
#'
#' inc(x) <- 1
#' x == 6
#'
#' dec(x) <- 2
#' x == 4
#'
NULL
