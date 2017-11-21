library(R6)
#' @export
Simulator <- R6Class(
  "Simulator",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  private = list(
    rewards = NULL
  ),
  public = list(
    agent_list = NULL,
    agent_n = NULL,
    animate = FALSE,
    initialize = function(agent_list, animate = FALSE) {
      self$agent_list = agent_list
      self$agent_n = length(agent_list)
      self$animate = animate
      self$reset
    },
    reset = function(){

      for (a in 1:agent_n) {
        agent_list[[a]]$reset()
      }
    },

    run = function(horizon=100L, simulations=1L) {

      score_vector   = rep(0, horizon*self$agent_n*simulations)
      optimal_vector = rep(0, horizon*self$agent_n*simulations)
      agent_vector   = rep(0, horizon*self$agent_n*simulations)
      t_vector       = rep(0, horizon*self$agent_n*simulations)
      sim_vector     = rep(0, horizon*self$agent_n*simulations)
      #arm_array     = rep(list(), horizon*agent_n*simulations)

      agent_instance =  matrix( list(), agent_n,simulations)
      bandit_instance = matrix( list(), agent_n,simulations)

      for (a in 1:agent_n) {
        for (s in 1:simulations) {
          agent_instance[a,s]  = list(self$agent_list[[a]]$clone())
          bandit_instance[a,s] = list(self$agent_list[[a]]$bandit$clone())
        }
      }

      counter = 1
      for (t in 1L:horizon) {
        for (a in 1:agent_n) {
          for (s in 1L:simulations) {
            context  = bandit_instance[[a,s]]$get_context()
            action   = agent_instance[[a,s]]$get_action(context)
            reward   = bandit_instance[[a,s]]$get_reward(action)
            agent_instance[[a,s]]$set_reward(reward$reward)

            agent_vector[counter] = names(self$agent_list)[a]
            t_vector[counter]     = t
            sim_vector[counter]   = s
            #arm_array[t,s, reward$arm] = arm_array[t,s, reward$arm] + 1L
            score_vector[counter] = score_vector[counter] + reward$reward
            if (reward$optimal) optimal_vector[counter] = optimal_vector[counter] + 1L
            counter = counter + 1
          }
          if (self$animate == TRUE && t %% 3 == 0) {
            #plot_results(setNames(list(rowMeans(score_vector), rowMeans(optimal_vector), arm_array),c("reward", "optimal", "agent")), t)
          }

        }
      }
      df = data.frame(setNames(list(sim_vector, t_vector, score_vector, optimal_vector, agent_vector),
                               c("sim","t","reward","optimal","agent")))
      df$agent = as.factor(df$agent)
      return(df)
    }
  )
)
