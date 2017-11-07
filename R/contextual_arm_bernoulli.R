library(R6)

#' @export
BernoulliArm <- R6Class(
  "BernoulliArm",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    p = NULL,
    initialize = function(p_in = NA, seed_in = NA) {
      self$p <- p_in
      if (!is.na(seed_in))
        set.seed(seed_in)
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
