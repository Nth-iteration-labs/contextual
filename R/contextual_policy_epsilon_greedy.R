library(R6)

#' @export
EpsilonGreedy <- R6Class(
  "EpsilonGreedy",
  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,
  public = list(
    epsilon = c(),
    counts  = c(),
    values  = c(),
    n_arms  = 0L,
    n  = 0.0,
    initialize = function(epsilon = NA) {
      self$epsilon <- epsilon
    },
    reset = function(arms) {
      self$n_arms <- arms
      self$counts <- rep(0L, times = arms)
      self$values <- rep(0.0, times = arms)
    },
    select_arm = function() {
      if (runif(1) > epsilon) {
        return( index_of_maximum(values) )
      } else {
        return(sample.int(n_arms, 1))
      }
    },
    update = function(chosen_arm, reward) {
      self$n = self$counts[chosen_arm] + 1.0
      self$counts[chosen_arm] = self$n
      self$values[chosen_arm] = ((self$n - 1.0) / self$n) * self$values[chosen_arm] + (1.0 / self$n) * reward
    }
  )
)

