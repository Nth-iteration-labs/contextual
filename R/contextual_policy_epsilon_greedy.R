library(R6)

#' @export
EpsilonGreedy <- R6Class(
  "EpsilonGreedy",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    epsilon = c(),
    counts = c(),
    values = c(),
    n_arms = 0L,
    n  = 0L,
    initialize = function(epsilon = NA, counts= c(), values = c()) { # why  set counts and values here???
      self$epsilon <- epsilon
      self$counts <- counts
      self$values <- values
    },
    set_arms = function(arms) {
      self$n_arms <- arms
      self$counts <- rep(0L, times = arms)
      self$values <- rep(0.0, times = arms)
    },
    select_arm = function() {
      if (runif(1) > epsilon) {
        return( which.is.max(values) )
      } else {
        return(sample.int(n_arms, 1))
      }
    },
    update = function(chosen_arm, reward) {
      self$n = self$counts[chosen_arm] + 1L
      self$counts[chosen_arm] = self$n
      value = self$values[chosen_arm]
      self$values[chosen_arm] = ((self$n - 1) / self$n) * value + (1 / self$n) * reward
    }
  )
)

which.is.max <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    return(sample(y, 1L))
  } else {
    return(y)
  }
}
