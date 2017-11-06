library(R6)


#' @export
EpsilonGreedy <- R6Class(
  "EpsilonGreedy",
  public = list(
    epsilon = NULL,
    counts = NULL,
    values = NULL,
    initialize = function(epsilon = NA, counts= c(), values = c()) { # why  set counts and values here???
      self$epsilon <- epsilon
      self$counts <- counts
      self$values <- values
    },
    set_arms = function(n_arms) {
      self$counts <- rep(0, times = n_arms)
      self$values <- rep(0.0, times = n_arms)
    },
    select_arm = function() {
      if (runif(1) > self$epsilon) {
        ret = sample(which(self$values == max(self$values)),1) # here, if multiple are top, random
      } else {
        ret = sample.int(length(self$values), 1)
      }
      ret
    },
    update = function(chosen_arm, reward) {
      self$counts[chosen_arm] = self$counts[chosen_arm] + 1
      n = self$counts[chosen_arm]
      value = self$values[chosen_arm]
      new_value = ((n - 1) / n) * value + (1 / n) * reward
      self$values[chosen_arm] = new_value
    }
  )
)
