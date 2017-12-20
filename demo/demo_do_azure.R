setwd("~/GitHub/contextual/demo")
#library(contextual)
source("dev.R")

set.seed(21L)                                                                   # set seed, to be able to repeat our tests with the same data

bandit <- SyntheticBandit$new(
  k = 3L,
  d = 3L,
  weight_distribution = "Uniform",
  reward_type =         "Bernoulli"
)

                     #d1  #d2  #d3
bandit$set_weights(c(0.9, 0.0, 0.1,  #k1                                        # override auto-generated weights
                     0.1, 0.9, 0.1,  #k2                                        # d stands for a feature,
                     0.9, 0.1, 0.9)) #k3                                        # k for an arm

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

simulations    <- 300L                                                          # define how many simulations
horizon        <- 100L                                                          # define how many each sim
simulation     <- SimulatorAzure$new(agents)                                    # let's see what our cunning agent can find out about the bandit
history        <- simulation$run(horizon, simulations)                          # go!

plot <- Plot$new()$set_external(T, 11, 6L)                                      # initialize plot.. TODO: change to within class
plot$grid(history)                                                              # plot the results...
