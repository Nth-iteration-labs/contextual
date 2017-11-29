library(R6)

# library(contextual)

# todo: theta setup, some mem stuff
# UCB1, exp3, exp4, epoch..
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


#bandit  = SyntheticBandit$new(k = 3L, d = 1, "uniform", "binary", "single")   # define a bandit, with k arms and d features - when d == 1 -> MAB
#bandit$set_weights( c(0.1,0.9,0.1) )

bandit  = SyntheticBandit$new(k = 3L, d = 3L, "uniform", "binary", "binary")   # define a bandit, with k arms and d features - when d == 1 -> MAB
bandit$set_weights( c(0.1,0.9,0.1,
                      0.9,0.1,0.1,
                      0.9,0.1,0.9) )                                           # warn if not conforming to ... k*d

policy = EpsilonGreedyPolicy$new(0.1, "\U190-greedy")                          # which policy do we want to test? here, epsilon_greedy, epsilon 0.1
agents$EG  = BasicAgent$new(policy, bandit)                                    # define an agent, who uses an policy, to find out more about a bandit

policy2 = LinUCBPolicy$new(1, "LinUCB")                                        # which policy do we want to test? here, linUCB, alpha 3
agents$LinUCB = LinUCBAgent$new(policy2, bandit)                               # define an agent, who uses an policy, to find out more about a bandit

policy3 = ThompsonSamplingPolicy$new(1,1, "ThompsonSampling")                  # which policy do we want to test? here, linUCB, alpha 3
agents$TS = BasicAgent$new(policy3, bandit)                                    # define an agent, who uses an policy, to find out more about a bandit

policy4 = RandomPolicy$new("Random")                                           # which policy do we want to test? here, linUCB, alpha 3
agents$Random = BasicAgent$new(policy4, bandit)                                # define an agent, who uses an policy, to find out more about a bandit

policy5 = Exp3Policy$new(0.1,"Exp3")                                             # which policy do we want to test? here, linUCB, alpha 3
agents$Exp3 = Exp3Agent$new(policy5, bandit)

simulations     = 100L                                                        # define how many simulations
horizon         = 200L
simulation      = Simulator$new( agents , FALSE)                               # let's see if our cunning agent can find out the bandit's prefs
history         = simulation$run( horizon, simulations )                       # run the simulation!

plot$grid(history)

#library(corrplot)
#col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", "cyan", "#007FFF", "blue", "#00007F"))
#corrplot(bandit$get_weights(), method="color",addCoef.col = "black", col = col4(10), cl.pos = "n", tl.pos = "n")
