library(R6)
#' @export
Exp3Policy <- R6Class(
  "Exp3Policy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    gamma = 0.1,
    name = "",

    initialize = function(gamma =  0.1, name = "Exp3" ) {
      self$gamma = gamma
      self$name  = name
    },

    categorical_draw = function(probs) {
      z = runif(1)
      cum_prob = 0.0
      lp = length(probs)
      for (i in 1:lp) {
         inc(cum_prob) <- probs[i]
         if (cum_prob > z) return(i)
      }
      return(sample(1:lp, 1))
    },

    get_action = function(agent) {
      probs = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        probs[arm] = (1 - self$gamma) * (agent$get_memory()$theta[arm] / sum(agent$get_memory()$theta))
      }
      inc(probs[arm]) <- (self$gamma) * (1.0 / agent$bandit$k)
      return(categorical_draw(probs))
    }
  )
)




