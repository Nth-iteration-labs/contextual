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
    list_level_to_vector = function(list_name, element_name)
    {
      unlist(lapply(list_name, `[[`, element_name) , FALSE, FALSE)
    },
    max_in_param = function(theta, param, equal_is_random = TRUE)
    {
      param <- list_level_to_vector(theta, param)
      max_in(param, equal_is_random)
    },
    max_in = function(x, equal_is_random = TRUE)
    {
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
    sum_of_param = function(theta, param)
    {
      sum(list_level_to_vector(theta, param))
    }
  )
)

#' Contextual Root Class
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
#'
#' \describe{
#'   \item{\code{list_level_to_vector(list_name, element_name)}}{
#'   Returns a vector of the values of all elements named \code{element_name}
#'   in multilevel list \code{list_name}.
#'   }
#' }
#'
#' \describe{
#'   \item{\code{max_in_param(theta, param, equal_is_random = TRUE)}}{
#'   Returns the index of the maximum value of the elements named \code{param}
#'   in multilevel list \code{theta}.
#'
#'   If there is a tie and \code{equal_is_random} is \code{TRUE},
#'   the index of one of the tied maxima is returned at random.
#'
#'   If \code{equal_is_random} is \code{FALSE},
#'   the maximum with the lowest index value is returned.
#'   }
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
#'   the maximum with the lowest index value is returned.
#'   }
#' }
#'
#' \describe{
#'   \item{\code{sum_of_param(theta, param)}}{
#'   Returns the sum of the values of al elements named \code{param} in multilevel list \code{theta}.
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
#' \code{\link{Plot}}
#'
#' @name Contextual
#' @family contextual classes
#' @examples
#' contextual <- Contextual$new()
#'
#' print(contextual$hash)
#'
#' contextual$max_in(c(71,94,10))
#'
#' theta <- list()
#' theta[[1]] <- list(param_a=1,param_b=2,param_c=3)
#' theta[[2]] <- list(param_a=4,param_b=5,param_c=6)
#'
#' contextual$sum_of_param(theta,"param_a")
#' contextual$max_in_param(theta,"param_a")
#' contextual$list_level_to_vector(theta, "param_a")
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
