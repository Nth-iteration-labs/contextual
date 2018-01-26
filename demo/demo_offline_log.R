########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### create a random log ################################

set.seed(12)

bandit      <- SyntheticBandit$new()
bandit$set_weights(matrix(
  c(0.9, 0.0, 0.1,
    0.1, 0.9, 0.1,
    0.1, 0.1, 0.9),
  nrow = 3,
  ncol = 3
))

policy      <- RandomPolicy$new()

agent       <- Agent$new(policy, bandit)

simulation  <-
  Simulator$new(
    agent,
    horizon = 400L,
    simulations = 400L,
    save_context = TRUE,
    save_theta = FALSE
  )

before <- simulation$run()

before$save_data("test.RData")

Plot$new()$grid(before)

######################## use the log to test a policy ##########################

log_S     <- History$new()
log_S$load_data("test.RData")

bandit      <- LiLogBandit$new(log_S, 3, 3)

policy      <- LinUCBPolicy$new(1.0)
agent       <- Agent$new(policy, bandit)
simulation  <- Simulator$new(agent, horizon = 400L, simulations = 400L, worker_max = 1 )

after <- simulation$run()

Plot$new()$grid(after)

if (file.exists("test.RData")) file.remove("test.RData")



