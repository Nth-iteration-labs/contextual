library(R6)

#' @export
BernoulliArm <- R6Class(
  "BernoulliArm",
  public = list(
    p = NULL,
    initialize = function(p = NA, seed = NA) {
      self$p <- p
      if (!is.na(seed))
        set.seed(seed)
      self$draw()
    },
    set_p = function(val) {
      self$p <- val
    },
    draw = function() {
      if (runif(1) > self$p) {
        ret = 0.0
      } else {
        ret = 1.0
      }
      ret
    }
  )
)
