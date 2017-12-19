# here, we did setweights, can also generate based on d/k, but check if either

setwd("~/GitHub/contextual/demo")
#library(contextual)
source("dev.R")

set.seed(21L)

bandit <- SyntheticBandit$new(
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)

                            #d1  #d2  #d3
bandit$set_weights(matrix(c(0.9, 0.0, 0.1,  #k1                                 # override auto-generated weights
                            0.1, 0.9, 0.1,  #k2                                 # d / nrow: how many features
                            0.9, 0.1, 0.9)  #k3                                 # k / ncol: how many arms
                            ,nrow = 3, ncol = 3))

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

ptm <- proc.time()

simulation     <- SimulatorParallel$new(agents,horizon = 100L, simulations = 100L)
history        <- simulation$run()

print(proc.time() - ptm)

plot <- Plot$new()$set_external(T, 11, 6L)                                      # initialize plot.. TODO: change to within class
plot$grid(history)                                                              # plot the results...

