library(R6)


#' @export
ContextualSimulation <- R6Class(

  "ContextualSimulation",

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

      scores  = matrix(0, horizon, length(self$agents))
      optimal = matrix(0, horizon, length(self$agents))

      for (iteration in 1:iterations) {
        self$reset()

        for (time_step in 1:horizon) {

          for (tested_agent in 1:length(self$agents)) {
            agent = self$agents[[tested_agent]]

            context = self$bandit$get_context()                ###
            action = agent$get_action(context)
            reward = self$bandit$get_reward(action, context)   ### hmm .. so is bandit the place to move the context?
            agent$set_reward(reward$arm, context)              ### intermingled!

            scores[time_step, tested_agent] = scores[time_step, tested_agent] + reward$reward

            if (reward$is_optimal) optimal[time_step, tested_agent] = (optimal[time_step, tested_agent]) + 1

          }
        }
      }
      return(setNames(list(scores / iterations, optimal / iterations),c("reward", "optimal")))
    }

  )
)


