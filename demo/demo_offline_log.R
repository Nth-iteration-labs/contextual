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
    horizon = 10L,
    simulations = 10L,
    save_context = TRUE,
    save_theta = FALSE
  )

before <- simulation$run()

before$save_data("test.RData")

#Plot$new()$grid(before)

print(before$data$reward)

######################## use the log to test a policy ##########################

log_S     <- History$new()
log_S$load_data("test.RData")

bandit      <- LiLogBandit$new(log_S, 3, 3)

policy      <- LinUCBPolicy$new(1.0)
agent       <- Agent$new(policy, bandit)
simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1 )

after <- simulation$run()

print(after$data$reward)

# Plot$new()$grid(after)

if (file.exists("test.RData")) file.remove("test.RData")


####

bandit <- AbstractBandit$new()

reward = list()
reward["reward"]  = 10
reward["choice"] = 1
reward["is_optimal"] = 0
reward["oracle"] = 1
reward["propensity"] = 0

action = list()
action$choice = 2
action$optimal_choice = 2

print(bandit$reward_to_list(action,1))
