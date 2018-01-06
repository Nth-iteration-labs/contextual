########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

set.seed(21)

ptm <- proc.time()

bandit      <- BasicBandit$new()

bandit      <- BasicBandit$new()$set_weights(c(0.1,0.9))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <- Simulator$new(agent, horizon = 2L, simulations = 2L)
history     <- simulation$run()

Plot$new()$grid(history)

print(sum(history$reward))

