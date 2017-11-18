
#' @export
SimulationSerial <- R6Class(

  "SimulationSerial",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  private = list(
    rewards = NULL
  ),

  public = list(

    bandit = NULL,
    agents = NULL,

    initialize = function(bandit, agent) {
      self$bandit = bandit
      self$agents = list(agent)
      self$reset()
    },

    reset = function(){
      self$bandit$reset()
      for (agent in self$agents) {
        agent$reset()
      }
    },

    run = function(horizon=100, simulations=1) {

      tested_agent = 1



      scores  = matrix(0, horizon, length(self$agents))
      optimal = matrix(0, horizon, length(self$agents))




      for (simulation in 1:simulations) {

        self$reset()

        for (time_step in 1:horizon) {

          context = self$bandit$get_context()                     # no context needed? or smarter here?

          action = agent$get_action(context)
          reward = self$bandit$get_reward(action)
          agent$set_reward(reward$reward)

          scores[time_step, tested_agent] = scores[time_step, tested_agent] + reward$reward
          if (reward$optimal) optimal[time_step, tested_agent] = (optimal[time_step, tested_agent]) + 1
        }

      }
      results = setNames(list(scores / simulations, optimal / simulations),c("reward", "optimal"))
      return(results)
    }
  )
)
