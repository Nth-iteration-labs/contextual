library(R6)
# library(contextual)

setwd("~/GitHub/contextual/scripts")

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/utility.R")
source("../R/bandit.R")

library(R6)

#' @export
Simulation <- R6Class(

  "Simulation",

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

    run = function(horizon=100, simulations=1) {

      scores  = matrix(0, horizon, length(self$agents))
      optimal = matrix(0, horizon, length(self$agents))

      for (simulation in 1:simulations) {
        self$reset()

        for (time_step in 1:horizon) {

          context = self$bandit$get_context()
          action = agent$get_action(context)
          reward = self$bandit$get_reward(action, context)
          agent$set_reward(reward$arm, context)

          scores[time_step, tested_agent] = scores[time_step, tested_agent] + reward$reward
          if (reward$is_optimal) optimal[time_step, tested_agent] = (optimal[time_step, tested_agent]) + 1
        }
      }
      return(setNames(list(scores / simulations, optimal / simulations),c("reward", "optimal")))
    }
  )
)

########################### basic  ###########################


source("../R/gradient.R")
source("../R/epsilon_greedy.R")

policy = EpsilonGreedyPolicy$new(0.1)                     # which policy do we want to test? here, epsilon_greedy, with epsilon 0.1

bandit = Bandit$new(k = 3, d = 1, 'binary')               # define a bandit, with k arms and d features - when d == 1, mab, otherwise cmab
agent  = GradientAgent$new(policy, bandit)                # define an agent, who uses an policy, to find out more about a bandit

simulations     = 100                                     # define how many simulations
horizon         = 100                                     # define the horizon (how many steps per simulation)

#simulation      = Simulation$new( bandit,  agent      )  # assign our bandit and and agent to a simulation arena..

#results         = simulation$run( horizon, simulation )  # run - lets see how well our agent is able to make use if his or her policy!

#matplot( results$optimal * 100,  type = "l", lty = 1, xlab = "Time Step", ylab = "Optimal Action", ylim = c(0,100))
#matplot( results$reward,         type = "l", lty = 1, xlab = "Time Step", ylab = "Average Reward")


########################### contextual ###########################

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


