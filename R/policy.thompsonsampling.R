library(R6)
#' @export
ThompsonSamplingPolicy <- R6Class(
  "ThompsonSamplingPolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    alpha = 1,
    beta = 1,
    name = "",
    mem = NULL, ################# this is no good! beeh!

    initialize = function(alpha = 1, beta =  1, name = "Thompson Sampling" ) {
      self$alpha = alpha
      self$beta  = beta
      self$name  = name
    },
    calc_rho = function(i) {

      rbeta(1, self$alpha + self$mem$succes.counts[i],
            self$beta + self$mem$choice.counts[i] - self$mem$succes.counts[i])
    },
    get_action = function(agent) {

      self$mem = agent$get_memory()
      mu = sapply(1:agent$bandit$k, function(i) self$calc_rho(i))
      return(index_of_max(mu));
    }
  )
)




