library(R6)

# library(contextual)

# TODO theta setup, some mem stuff
# mult and div apply
# do sapply or not exp3/ts
# UCB1, exp3, exp4, epoch..
# create data from existing
# online and offline bandits
# Then more interesting stuff from Maurits
# display baysian and ucb live changes
# add/remove arms
# changes over time
# documentation and article(s)
# parallelization
# parameter tuning?
# write online ..

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

setwd("~/GitHub/contextual/scripts")

source("../R/utility.R")
source("../R/simulator.R")
source("../R/plot.R")
source("../R/bandit.synthetic.R")
source("../R/policy.linucb.R")
source("../R/policy.thompsonsampling.R")
source("../R/policy.epsilongreedy.R")
source("../R/policy.random.R")
source("../R/policy.exp3.R")
source("../R/agent.basic.R")
source("../R/agent.linucb.R")
source("../R/agent.exp3.R")

plot = Plot$new()
plot$set_external(T,11,6)

agents = list()                                                                # to keep track of our agents
set.seed(21L)                                                                  # set seed, to be able to repeat our tests with the same data

# MAB Bandit
# bandit  = SyntheticBandit$new(k = 3L, d = 1, "uniform", "binary", "single")  # define a bandit, with k arms and d features
# bandit$set_weights( c(0.1,0.9,0.1) )

# cMAB Bandit
bandit  = SyntheticBandit$new(k = 3L, d = 3L, "uniform", "binary", "binary")   # define a bandit, with k arms and d features
bandit$set_weights( c(0.1,0.9,0.1,                                             # override auto-generated weights
                      0.9,0.1,0.1,
                      0.9,0.1,0.9) )                                           # TODO: warn if not conforming to ... k*d

policyEG       = EpsilonGreedyPolicy$new(0.1, "\U190-greedy")                  # which policy do we want to test?
agents$EG      = BasicAgent$new(policyEG, bandit)                              # define an agent, who uses an policy, to find out more about a bandit

policyTS       = ThompsonSamplingPolicy$new(1,1, "ThompsonSampling")           # which policy do we want to test?
agents$TS      = BasicAgent$new(policyTS, bandit)                              # define an agent, who uses an policy, to find out more about a bandit

policyRandom   = RandomPolicy$new("Random")                                    # which policy do we want to test?
agents$Random  = BasicAgent$new(policyRandom, bandit)                          # define an agent, who uses an policy, to find out more about a bandit

policyExp3     = Exp3Policy$new(0.1,"Exp3")                                    # which policy do we want to test?
agents$Exp3    = Exp3Agent$new(policyExp3, bandit)                             # define an agent, who uses an policy, to find out more about a bandit

policyLinUCB   = LinUCBPolicy$new(1, "LinUCB")                                 # which policy do we want to test?
agents$LinUCB  = LinUCBAgent$new(policyLinUCB, bandit)                         # define an agent, who uses an policy, to find out more about a bandit

simulations    = 50L                                                           # define how many simulations
horizon        = 100L                                                          # define how many each sim
simulation     = Simulator$new( agents , FALSE)                                # let's see what our cunning agent can find out about the bandit
history        = simulation$run( horizon, simulations )                        # go!

plot$grid(history)
