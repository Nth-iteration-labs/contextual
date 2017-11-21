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
    agent_list = NULL,
    animate = FALSE,
    initialize = function(agent_list, animate = FALSE) {
      #self$agent_list[[agent_nr]]$bandit = bandit
      self$agent_list = agent_list
      self$animate = animate
    },
    reset = function(){
      for (agent_nr in 1:length(self$agent_list)) {
        self$agent_list[[agent_nr]]$bandit$reset()
        self$agent_list[[agent_nr]]$reset()
      }
    },
    run = function(horizon=100L, simulations=1L) {
      scores_array  = array(0, dim = c( horizon, simulations, length(self$agent_list ))   )                     # to array, for each agent, later..
      optimal_array = array(0, dim = c( horizon, simulations, length(self$agent_list )))
      arm_array      = array(0, dim = c( horizon, simulations, bandit$k, length(self$agent_list)))
      agent_vector   = matrix(list(),simulations,length(self$agent_list))
      bandit_vector  = matrix(list(),simulations,length(self$agent_list))

      for (sim_nr in 1:simulations) {
        for (agent_nr in 1:length(self$agent_list)) {
          agent_vector[sim_nr,agent_nr]  = list(self$agent_list[[agent_nr]]$clone())             # really? clone? can not be faster?
          bandit_vector[sim_nr,agent_nr] = list(self$agent_list[[agent_nr]]$bandit$clone())
        }
      }
      for (time_step in 1L:horizon) {
        for (sim_nr in 1L:simulations) {
          for (agent_nr in 1:length(self$agent_list)) {

            context  = bandit_vector[[sim_nr,agent_nr]]$get_context()                     # no context needed in mab? smarter here?
            action   = agent_vector[[sim_nr,agent_nr]]$get_action(context)                # this is ok
            reward   = bandit_vector[[sim_nr,agent_nr]]$get_reward(action)                # negative reward?

            agent_vector[[sim_nr,agent_nr]]$set_reward(reward$reward)
            arm_array[time_step,sim_nr, reward$arm,agent_nr] = arm_array[time_step,sim_nr, reward$arm, agent_nr] + 1L
            scores_array[time_step,sim_nr,agent_nr] = scores_array[time_step,sim_nr,agent_nr] + reward$reward
            if (reward$optimal) optimal_array[time_step, sim_nr, agent_nr] = optimal_array[time_step, sim_nr,agent_nr] + 1L
          }
        }
        if (self$animate == TRUE && time_step %% 3 == 0) {
          #plot_results(setNames(list(rowMeans(scores_array), rowMeans(optimal_array), arm_array),c("reward", "optimal", "arm")), time_step)
        }
      }
      return(setNames(list(list(scores_array), list(optimal_array), list(arm_array)),c("reward", "optimal", "arm")))
    }
  )
)

# source("../R/linucb_agent.R")
# source("../R/linUCB.R")
#
# external_graphs(T,11,6)                                             # set to external device if Rstudio
#
# set.seed(20)                                                        # set seed, to be able to repeat our tests with the same data
# policy = LinUCBPolicy$new(0.5)                                      # which policy do we want to test? here, linUCB, alpha 0.5
# bandit = Bandit$new(k = 3L, d = 3L, 'gaussian')                     # define a bandit, with k arms and d features - when d == 1 -> MAB
# agent  = LinUCBAgent$new(policy, bandit)                            # define an agent, who uses an policy, to find out more about a bandit
# simulations     = 100L                                              # define how many simulations
# horizon         = 100L                                              # define the horizon (how many steps per simulation)
# simulation      = SimulationVectorized$new( bandit,  agent , T)     # assign a bandit and our cunning agent to a simulation arena..
# results         = simulation$run( horizon, simulations )            # lets see how well our agent is able to make use if his or her policy!
#
# plot_results(results)                                               # plot the results

##################    mab    #####################
source("../R/agent.R")
source("../R/epsilon_greedy.R")
source("../R/linucb_agent.R")
source("../R/linUCB.R")

agent_list  = list()

# together? decouple?

external_graphs(T,11,6)                                             # set to external device if Rstudio

set.seed(20)                                                        # set seed, to be able to repeat our tests with the same data
bandit           = Bandit$new(k = 3L, d = 1L, 'gaussian')           # define a bandit, with k arms and d features - when d == 1 -> MAB

policy_one = EpsilonGreedyPolicy$new(0.1)                     # which policy do we want to test? here, epsilon_greedy, epsilon 0.1
agent_list[[1]]  = Agent$new(policy_one, bandit$clone())                # define an agent, who uses an policy, to find out more about a bandit

policy_two = policy = LinUCBPolicy$new(0.5)                   # define a bandit, with k arms and d features - when d == 1 -> MAB
agent_list[[2]]  = LinUCBAgent$new(policy_two, bandit$clone())

simulations     = 100L                                              # define how many simulations
horizon         = 100L                                              # define the horizon (how many steps per simulation)
simulation      = SimulationVectorized$new( agent_list , T)             # assign a bandit and our cunning agent to a simulation arena..!!!!!!!!!!!!!!
results         = simulation$run( horizon, simulations )            # lets see how well our agent is able to make use if his or her policy!

plot_results(results)                                               # plot the results

