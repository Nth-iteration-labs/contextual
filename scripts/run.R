library(R6)
library(dplyr)
# library(contextual)
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

setwd("~/GitHub/contextual/scripts")

source("../R/utility.R")
source("../R/simulator.R")
source("../R/bandit.synthetic.R")
source("../R/agent.linucb.R")
source("../R/policy.linucb.R")
source("../R/agent.basic.R")
source("../R/policy.epsilongreedy.R")

external_graphs(T,11,6)                                             # plot to external device
agents = list()                                                     # to keep track of our agents
set.seed(21L)                                                       # set seed, to be able to repeat our tests with the same data

bandit  = SyntheticBandit$new(k = 3L, d = 1L)                       # define a bandit, with k arms and d features - when d == 1 -> MAB

policy = EpsilonGreedyPolicy$new(0.1, "\U190Greedy .1")             # c(0.1,0.2 etc) which policy do we want to test? here, epsilon_greedy, epsilon 0.1
agents$EpsilonGreedy  = Agent$new(policy, bandit)                   # define an agent, who uses an policy, to find out more about a bandit

policy = EpsilonGreedyPolicy$new(0.6, "\U190Greedy .6")             # which policy do we want to test? here, epsilon_greedy, epsilon 0.1
agents$EpsilonGreedy2 = Agent$new(policy, bandit)                   # define an agent, who uses an policy, to find out more about a bandit

#policy4 = LinUCBPolicy$new(3)                                      # which policy do we want to test? here, linUCB, alpha 0.5
#agents$LinUCB = = LinUCBAgent$new(policy4, bandit)                 # define an agent, who uses an policy, to find out more about a bandit

simulations     = 100L                                              # define how many simulations
horizon         = 100L                                              # define the horizon (how many steps per simulation)
simulation      = Simulator$new( agents , T)                        # let's see if our cunning agent can find out the bandit's prefs

history         = simulation$run( horizon, simulations )

