########################### package dev helpers ################################
#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### package dev helpers ################################

set.seed(12)

ptm <- proc.time()

bandit      <- SyntheticBandit$new(  weight_distribution = "Uniform",
                                     reward_type =         "Bernoulli")$generate_weights(3,3)

policy      <- EpsilonGreedyPolicy$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 200L)

history     <- simulation$run()

Plot$new()$set_external(T, 11, 6L)$grid(history)

print(proc.time() - ptm)


######################## playing around with logs ##############################

history$save_data("save.RData")

history     <- History$new()
history$load_data("save.RData")

history_dt  <- history$get_data_table()
history_df  <- history$get_data_frame()
