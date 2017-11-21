library(R6)
#' @export
EpsilonGreedyPolicy <- R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    epsilon = 0.1,
    initialize = function(epsilon = 0.1) {
      self$epsilon = epsilon
    },
    get_action = function(agent){
      if (runif(1) < self$epsilon) {
        return(sample.int(agent$bandit$k, 1))
      } else {
        return(index_of_max(agent$get_memory()))
      }
    }

  )
)
