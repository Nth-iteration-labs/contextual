library(R6)
#' @export
ThompsonSamplingPolicy <- R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    alpha = 1,
    beta = 1,
    name = "",
    initialize = function(alpha = 1, beta =  1, name = "Thompson Sampling" ) {
      self$alpha = alpha
      self$beta  = beta
      self$name  = name
    },
    get.action = function(agent,context) {
      mu = rep(0.0, agent$bandit$k)
      for (arm in 1:agent$bandit$k) {
        mu[arm] =  rbeta(1, self$alpha + agent$get.memory()$succes.counts[arm],
                         self$beta + agent$get.memory()$choice.counts[arm] - agent$get.memory()$succes.counts[arm])
      }
      return(index.of.max(mu));


    }
  )
)




