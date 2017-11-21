library(R6)

# library(contextual)
setwd("~/GitHub/contextual/scripts")
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/utility.R")
source("../R/bandit.R")
source("../R/simulate.R")

source("../R/linucb_agent.R")
source("../R/linUCB.R")

# plot to external device
external_graphs(F,11,6)

set.seed(2)                                                         # set seed, to be able to repeat our tests with the same data
policy = LinUCBPolicy$new(0.5)                                      # which policy do we want to test? here, linUCB, alpha 0.5
bandit = Bandit$new(k = 3L, d = 10L, 'gaussian')                    # define a bandit, with k arms and d features - when d == 1 -> MAB
agent  = LinUCBAgent$new(policy, bandit)                            # define an agent, who uses an policy, to find out more about a bandit
simulations     = 100L                                              # define how many simulations
horizon         = 100L                                              # define the horizon (how many steps per simulation)
simulation      = SimulationVectorized$new( bandit,  agent , F)     # assign a bandit and our cunning agent to a simulation arena..
results         = simulation$run( horizon, simulations )            # lets see how well our agent is able to make use if his or her policy!

plot_results(results)                                               # plot the results





# source("../R/agent.R")
# source("../R/epsilon_greedy.R")

# set.seed(2)                                                         # set seed, to be able to repeat our tests with the same data
# policy = EpsilonGreedyPolicy$new(0.1)                               # which policy do we want to test? here, epsilon_greedy, epsilon 0.1
# bandit = Bandit$new(k = 3L, d = 10L, 'gaussian')  # bandit... gen or data gen                   # define a bandit, with k arms and d features - when d == 1 -> MAB
# agent  = Agent$new(policy, bandit)                                  # define an agent, who uses an policy, to find out more about a bandit
# simulations     = 100L                                              # define how many simulations
# horizon         = 100L                                              # define the horizon (how many steps per simulation)
# simulation      = SimulationVectorized$new( bandit,  agent , F)     # assign a bandit and our cunning agent to a simulation arena..
# results         = simulation$run( horizon, simulations )            # lets see how well our agent is able to make use if his or her policy!
