library(R6)


#' @export
BasicEnvironment <- R6Class(

  "BasicEnvironment",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  private = list(
    rewards = NULL
  ),
  public = list(

    bandit = NULL,
    agents = NULL,
    label = NULL,

    initialize = function(bandit = NA, agents = NA, label='Multi-Armed Bandit') {
      self$bandit = bandit
      self$agents = agents
      self$label  = label
    },

    reset = function(){
      self$bandit$reset()
      for (agent in self$agents) {
        agent$reset()
      }
    },

    run = function(trials=100, experiments=1) {

      scores  = matrix(0,trials, length(self$agents))
      optimal = matrix(0,trials, length(self$agents))

      for (a in 1:experiments) {
        self$reset()

        for (t in 1:trials) {
          for (i in 1:length(self$agents)) {

            action = self$agents[[i]]$choose()                  ########################### get_action
            get_reward = self$bandit$pull(action)               ########################### get_reward
            self$agents[[i]]$observe(get_reward$reward)         ########################### set_reward

            scores[t, i] = scores[t, i] + get_reward$reward

            if (get_reward$is_optimal) optimal[t, i] = optimal[t, i] + 1

          }
        }
      }
      return(setNames(list(scores / experiments, optimal / experiments),c("reward", "optimal")))
    }

  )
)


