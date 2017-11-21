library(R6)
library(ggplot2)
# library(contextual)
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

setwd("~/GitHub/contextual/scripts")

source("../R/utility.R")
source("../R/simulator.R")
source("../R/bandit.R")
source("../R/agent.linucb.R")
source("../R/policy.linucb.R")
source("../R/agent.basic.R")
source("../R/policy.epsilongreedy.R")

external_graphs(F,11,6)                                             # plot to external device
agent_list = list()                                                 # to keep track of our agents

set.seed(20)                                                        # set seed, to be able to repeat our tests with the same data

bandit1 = Bandit$new(k = 3L, d = 4L, 'gaussian')                    # define a bandit, with k arms and d features - when d == 1 -> MAB
policy1 = EpsilonGreedyPolicy$new(0.2)                              # which policy do we want to test? here, epsilon_greedy, epsilon 0.1
agent1 = Agent$new(policy1, bandit1)                                # define an agent, who uses an policy, to find out more about a bandit
agent_list$EpsilonGreedy= agent1

bandit2 = Bandit$new(k = 3L, d = 4L, 'gaussian')                    # define a bandit, with k arms and d features - when d == 1 -> MAB
policy2 = LinUCBPolicy$new(3)                                       # which policy do we want to test? here, linUCB, alpha 0.5
agent2  = LinUCBAgent$new(policy2, bandit2)                         # define an agent, who uses an policy, to find out more about a bandit
agent_list$LinUCB = agent2

simulations     = 1000L                                             # define how many simulations
horizon         = 1000L                                             # define the horizon (how many steps per simulation)
simulation      = Simulator$new( agent_list , F)                    # let's see if our cunning agent can find out the bandit's prefs
results         = simulation$run( horizon, simulations )            # lets see how well our agent is able to make use if his or her policy!

aggr            = aggregate(results[, 3], list(results$t, results$agent ), mean)
names(aggr)     = c("T","Agent","Reward")
p = ggplot(data = aggr, aes(x = T, y = Reward, group = Agent, colour = Agent)) + geom_line()
print(p)
