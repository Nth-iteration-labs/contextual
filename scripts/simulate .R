library(R6)


# library(contextual)
setwd("~/GitHub/contextual/scripts")
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/utility.R")
source("../R/bandit.R")

SimulationVectorized <- R6Class(

  "SimulationVectorized",

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
      arm_array = array(0, dim = c( horizon, simulations, bandit$k))

      agent_vector   <- vector("list",simulations)
      bandit_vector  <- vector("list",simulations)

      for (i in 1:simulations) {
        agent_vector[[i]]  <- self$agent$clone()
        bandit_vector[[i]] <- self$bandit$clone()
      }

      if (self$animate == TRUE) {
        par(mfrow = c(2,1))
      }

      #foreach(time_step = 1:horizon) %dopar% %:% { etc
      #  foreach(sim_nr = 1:simulations) %do% {

      for (time_step in 1L:horizon) {
        for (sim_nr in 1L:simulations) {

          context  = bandit_vector[[sim_nr]]$get_context()                     # no context needed in mab? smarter here?
          action   = agent_vector[[sim_nr]]$get_action(context)
          reward   = bandit_vector[[sim_nr]]$get_reward(action)                # negative reward?

          agent_vector[[sim_nr]]$set_reward(reward$reward)

          arm_array[time_step,sim_nr, reward$arm] = arm_array[time_step,sim_nr, reward$arm] + 1L
          scores_matrix[time_step,sim_nr] = scores_matrix[time_step,sim_nr] + reward$reward
          if (reward$optimal) optimal_matrix[time_step, sim_nr] = optimal_matrix[time_step, sim_nr] + 1L
        }

        if (self$animate == TRUE ) {
          plot_results(setNames(list(rowMeans(scores_matrix), rowMeans(optimal_matrix), arm_array),c("reward", "optimal", "arm")), time_step)
        }
      }
      results = setNames(list(rowMeans(scores_matrix), rowMeans(optimal_matrix), arm_array),c("reward", "optimal", "arm"))

      return(results)
    }
  )
)



##################  basic  #####################

source("../R/agent.R")
source("../R/epsilon_greedy.R")

external_graphs(T,11,6)                                                # set to external device if Rstudio

set.seed(20)                                                           # set seed, to be able to repeat our tests with the same data
policy = EpsilonGreedyPolicy$new(0.1)                                  # which policy do we want to test? here, epsilon_greedy, with epsilon 0.1
bandit = Bandit$new(k = 10L, d = 1L, 'gaussian')                       # define a bandit, with k arms and d features - when d == 1 -> MAB, else cMAB
agent  = Agent$new(policy, bandit)                                     # define an agent, who uses an policy, to find out more about a bandit
simulations     = 200L                                                 # define how many simulations
horizon         = 300L                                                 # define the horizon (how many steps per simulation)
simulation      = SimulationVectorized$new( bandit,  agent , TRUE)    # assign a bandit and our cunning agent to a simulation arena..
results         = simulation$run( horizon, simulations )               # run - lets see how well our agent is able to make use if his or her policy!

plot_results(results)                                                  # plot the results

##################  contextual  ##################

#source("../R/agent.R")
#source("../R/linucb.R")

# tbd: still need memory, with theta part and general part, plus history..

#bandit = Bandit$new(k = 3, d = 10, 'binary')              # define a bandit, with k arms and d features
#linUCB = LinUCBPolicy$new(0.4)                            # define a policy, with parameter (here, LinUCB alpha - 0.4)
#agent  = Agent$new(linUCB, bandit)                        # define an agent, who uses an policy, to find out more about a bandit

#simulations     = 100                                     # define how many simulations
#horizon         = 100                                     # define the horizon (how many steps per simulation)

#simulation      = Simulation$new( bandit,  agent      )   # assign a bandit and an agent to a simulation
#results         = simulation$run( horizon, simulation )   # run the simulation - lets see how well our agent does, making use if his or her policy!

#matplot( results$optimal * 100,  type = "l", lty = 1, xlab = "Time Step", ylab = "Optimal Action", ylim = c(0,100))
#matplot( results$reward,         type = "l", lty = 1, xlab = "Time Step", ylab = "Average Reward")


