library(R6)

#' @export
BernoulliArm <- R6Class(
  "BernoulliArm",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    p = 0.0,
    initialize = function(p = NA) {
      self$p <- p
    },
    set_p = function(val) {
      self$p <- val
    },
    draw = function() {
      if (runif(1) > p) {
        return(0.0)
      } else {
        return(1.0)
      }
    }
  )
)
