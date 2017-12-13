########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

############################# basic simulation #################################

bandit      <- SyntheticBandit$new(k = 3L, d = 1L)$set_weights(c(0.1, 0.1, 0.9))

policy      <- EpsilonGreedyPolicy$new(0.1)

agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent)
history     <- simulation$run(horizon = 100L, simulations = 100L)

Plot$new()$grid(history)

################################################################################
