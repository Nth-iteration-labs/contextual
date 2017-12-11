setwd("~/GitHub/contextual/demo")
#library(contextual)
source("dev.R")

plot <- Plot$new()                                                              # Initialize live plot. TODO: change to within class.
plot$set_external(T, 11, 6L)                                                    # Set plot to external window when in Rstudio.

agents <- list()                                                                # A list to keep track of our agents
set.seed(21L)                                                                   # Set a seed, to compare simulation more easily.

bandit  = SyntheticBandit$new(
  k = 3L,
  d = 1L,
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)
                     #d1
bandit$set_weights(c(0.2,  #k1                                                  # Override auto-generated random weights
                     0.4,  #k2                                                  # d is the number of feature,
                     0.8)) #k3                                                  # k is the number of arms.

policyEG       <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")                  # which policy do we want to test?
agents$EG      <- Agent$new(policyEG, bandit)                                   # Define an agent,
                                                                                # who applies a policy,
                                                                                # to find out more about a bandit
                                                                                # in a certain context.

policyTS       <- ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")
agents$TS      <- Agent$new(policyTS, bandit)

policyRandom   <- RandomPolicy$new("Random")
agents$Random  <- Agent$new(policyRandom, bandit)

simulations    <- 100L                                                          # Now define how many simulations we'll run..
horizon        <- 100L                                                          # and how many time steps per simulation.

simulation     <- SimulatorBasic$new(agents, animate = TRUE, animate_step = 1)  # Let's see what our cunning agents
                                                                                # are able to deduct from the bandit's behaviour.
history        <- simulation$run(horizon, simulations)                          # go!








