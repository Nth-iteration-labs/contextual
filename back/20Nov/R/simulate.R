Simulate <- R6Class(
  "Simulate",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  private = list(
    rewards = NULL
  ),
  public = list(
    bandit = NULL,
    agent = NULL,
    animate = FALSE,
    initialize = function(bandit, agent, animate = FALSE) {
      self$bandit = bandit
      self$agent = agent
      self$animate = animate
    },
    reset = function(){
      self$bandit$reset()
      self$agent$reset()
    },
    run = function(horizon=100L, simulations=1L) {
      scores_matrix  = matrix(0, horizon, simulations)                        # to array, for each agent, later..
      optimal_matrix = matrix(0, horizon, simulations)
      arm_array      = array(0, dim = c( horizon, simulations, bandit$k))
      agent_vector   = vector("list",simulations)
      bandit_vector  = vector("list",simulations)
      for (i in 1:simulations) {
        agent_vector[[i]]  = self$agent$clone()
        bandit_vector[[i]] = self$bandit$clone()
      }
      for (time_step in 1L:horizon) {
        for (sim_nr in 1L:simulations) {
          context  = bandit_vector[[sim_nr]]$get_context()                     # no context needed in mab? smarter here?
          action   = agent_vector[[sim_nr]]$get_action(context)                # this is ok
          reward   = bandit_vector[[sim_nr]]$get_reward(action)                # negative reward?

          agent_vector[[sim_nr]]$set_reward(reward$reward)
          arm_array[time_step,sim_nr, reward$arm] = arm_array[time_step,sim_nr, reward$arm] + 1L
          scores_matrix[time_step,sim_nr] = scores_matrix[time_step,sim_nr] + reward$reward
          if (reward$optimal) optimal_matrix[time_step, sim_nr] = optimal_matrix[time_step, sim_nr] + 1L
        }
        if (self$animate == TRUE && time_step %% 3 == 0) {
          plot_results(setNames(list(rowMeans(scores_matrix), rowMeans(optimal_matrix), arm_array),c("reward", "optimal", "arm")), time_step)
        }
      }
      return(setNames(list(rowMeans(scores_matrix), rowMeans(optimal_matrix), arm_array),c("reward", "optimal", "arm")))
    }
  )
)
