library(R6)

#' @export
EpsilonGreedyPolicy <- R6Class(

  "EpsilonGreedyPolicy",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    epsilon = NULL,

    initialize = function(epsilon = NA) {
      self$epsilon = epsilon
    },

    choose = function(agent){
      agent_value_estimates = agent$get_value_estimates()
      if (runif(1) < self$epsilon) {
        return(sample.int(length(agent_value_estimates), 1))
      } else {
        action = index_of_max(agent_value_estimates)
        return(action)
      }
    }

  )
)

