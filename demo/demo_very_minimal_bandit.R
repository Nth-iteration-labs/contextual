########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

set.seed(21)

ptm <- proc.time()

bandit      <- AbstractBandit$new()

bandit      <- AbstractBandit$new()$set_weights(c(0.1,0.9))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorParallel$new(agent, horizon = 2L, simulations = 2L)
history     <- simulation$run()

Plot$new()$set_external(T, 11, 6L)$grid(history)

print(sum(history$reward))

