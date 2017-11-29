library(R6)
library(data.table)
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
    horizon = 100L,
    simulations = 1L,
    history = data.table(),

    initialize = function(agent_list, animate = FALSE) {
      self$agent_list = agent_list
      self$agent_n = length(agent_list)
      self$animate = animate
      self$reset
    },
    reset = function(){
      for (a in 1L:agent_n) {
        agent_list[[a]]$reset()
      }
    },
    run = function(horizon=100L, simulations=1L) {

      self$horizon = horizon
      self$simulations = simulations
      n = self$horizon*self$agent_n*self$simulations

      self$history = data.table(
        reward          = rep(0L,     n), #1
        optimal         = rep(0L,     n), #2
        agent           = rep("",     n), #3
        t               = rep(0L,     n), #4
        sim             = rep(0L,     n), #5
        arm             = rep(0L,     n)  #6
      )

      agent_instance =  matrix( list(), agent_n,simulations)
      bandit_instance = matrix( list(), agent_n,simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent_instance[a,s]  = list(self$agent_list[[a]]$clone())
          bandit_instance[a,s] = list(self$agent_list[[a]]$bandit$clone())
        }
      }

      counter = 1L

      for (t in 1L:self$horizon) {
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {

            context  = bandit_instance[[a,s]]$get_context()        # works at this t for bandit, k * d
            action   = agent_instance[[a,s]]$get_action(context)   # agent chooses an arm k, knowing context d
            reward   = bandit_instance[[a,s]]$get_reward(action)   # see how the bandit rewards us
            agent_instance[[a,s]]$set_reward(reward$reward)        # agent remembers reward for next round
            set(self$history,counter,4L,t)
            set(self$history,counter,5L,s)
            set(self$history,counter,6L,action)
            set(self$history,counter,1L,reward$reward)
            set(self$history,counter,3L,agent_instance[[a,s]]$policy$name)
            #set(self$history,counter,7L,bandit_instance[[a,s]]$get_weights())
            if (reward$optimal) set(self$history,counter,2L,1L)

            counter = counter + 1L
          }
        }
        if (self$animate == TRUE && t %% 2 == 0) plot$grid(history[t != 0L])  # xlim = c(0,horizon))
      }
      return(history)
    }
  )
)
