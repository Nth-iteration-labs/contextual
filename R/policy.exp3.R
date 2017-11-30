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
    categorical.draw = function(probs) {
      z = runif(1)
      cum.prob = 0.0
      lp = length(probs)
      for (i in 1:lp) {
         inc(cum.prob) <- probs[i]
         if (cum.prob > z) return(i)
      }
      return(sample(1:lp, 1))
    },
    get.action = function(agent,context) {
      probs = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        probs[arm] = (1 - self$gamma) * (agent$get.memory()$theta[arm] / sum(agent$get.memory()$theta))
      }
      inc(probs[arm]) <- (self$gamma) * (1.0 / agent$bandit$k)
      return(categorical.draw(probs))
    }
  )
)




