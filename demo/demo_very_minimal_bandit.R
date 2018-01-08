########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

set.seed(21)

ptm <- proc.time()

bandit      <- BasicBandit$new()

bandit      <- BasicBandit$new()
bandit$set_weights(c(0.1, 0.9))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <-
  Simulator$new(agent, horizon = 100L, simulations = 100L)
history     <- simulation$run()

Plot$new()$grid(history)

############


#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

bandit      <- BasicBandit$new()
bandit$set_weights(matrix(c(0.1, 0.9, 0.1, 0.5, 0.1, 0.1), 3, 3))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <-
  Simulator$new(
    agent,
    horizon = 30L,
    simulations = 30L,
    worker_max = 1
  )
context <- bandit$get_context()
history     <- simulation$run()

reward = list()
reward["reward"]  = 1
reward["optimal"] = 0

history$save_agent(
  counter = 1,
  t = 1,
  action = 2,
  reward = reward,
  policy_name = "EpsilonGreedy",
  s = 31
)

Plot$new()$grid(history)
