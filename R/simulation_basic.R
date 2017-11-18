library(R6)


#' @export
BasicSimulation <- R6Class(

  "BasicSimulation",

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

    initialize = function(comparison) {
      self$bandit = comparison$bandit
      self$agents = comparison$agents
      self$label  = comparison$label
    },

    reset = function(){
      self$bandit$reset()
      for (agent in self$agents) {
        agent$reset()
      }
    },

    run = function(horizon=100, iterations=1) {

      scores  = matrix(0,horizon, length(self$agents))
      optimal = matrix(0,horizon, length(self$agents))

      for (a in 1:iterations) {
        self$reset()

        for (t in 1:horizon) {

          for (i in 1:length(self$agents)) {
            agent = self$agents[[i]]

            ########################################
            action = agent$get_action()
            reward = self$bandit$get_reward(action)
            agent$set_reward(reward$reward)
            ########################################

            scores[t, i] = scores[t, i] + reward$reward
            if (reward$is_optimal) optimal[t, i] = optimal[t, i] + 1

          }
        }
      }
      return(setNames(list(scores / iterations, optimal / iterations),c("reward", "optimal")))
    }

  )
)


