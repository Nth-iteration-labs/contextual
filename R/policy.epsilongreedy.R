library(R6)
#' @export
EpsilonGreedyPolicy <- R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    epsilon = 0.1,
    name = "",
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy" ) {
      self$epsilon = epsilon
      self$name = name
    },
    get_action = function(agent){
      if (runif(1) < self$epsilon) {
        return(sample.int(agent$bandit$k, 1))
      } else {
        memory = agent$get_memory()
        return(index_of_max(memory$theta))
      }
    }
  )
)
