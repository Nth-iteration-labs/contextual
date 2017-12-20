########################### package dev helpers ################################
#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")
########################### package dev helpers ################################

set.seed(12)

bandit      <- SyntheticBandit$new()$generate_weights(3)
policy      <- EpsilonGreedyPolicy$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

Plot$new()$set_external(T, 11, 6L)$grid(history)
