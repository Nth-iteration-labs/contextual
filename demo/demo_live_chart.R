#library(contextual)
source("dev.R")
setwd("~/GitHub/contextual/demo")

plot = Plot$new()                                                               # initialize plot.. TODO: change to within class
plot$set_external(T, 11, 6L)                                                    # set external for Rstudio

agents = list()                                                                 # to keep track of our agents
set.seed(21L)                                                                   # set seed, to be able to repeat our tests with the same data

bandit  = SyntheticBandit$new(
  k = 3L,
  d = 1L,
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)

                     #d1
bandit$set_weights(c(0.1,  #k1                                                  # override auto-generated weights
                     0.1,  #k2                                                  # d stands for a feature,
                     0.9)) #k3                                                  # k for an arm

policyEG       = EpsilonGreedyPolicy$new(0.1, "\U190-greedy")                   # which policy do we want to test?
agents$EG      = BasicAgent$new(policyEG, bandit)                               # define an agent, who uses an policy, to find out more about a bandit

policyRandom   = RandomPolicy$new("Random")                                     # which policy do we want to test?
agents$Random  = BasicAgent$new(policyRandom, bandit)                           # define an agent, who uses an policy, to find out more about a bandit

simulations    = 300L                                                           # define how many simulations
horizon        = 100L                                                           # define how many each sim
simulation     = SimulatorBasic$new(agents, animate = TRUE, animate_step = 1)   # let's see what our cunning agent can find out about the bandit
history        = simulation$run(horizon, simulations)                           # go!
