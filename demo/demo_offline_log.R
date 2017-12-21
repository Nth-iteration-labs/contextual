########################### package dev helpers ################################
#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")
########################### package dev helpers ################################

set.seed(12)


set.seed(12)

bandit      <- SyntheticBandit$new()

bandit$set_weights(matrix(c(0.9, 0.0, 0.1,  #k1                                 # d / nrow: how many features
                            0.1, 0.9, 0.1,  #k2                                 # k / ncol: how many arms
                            0.9, 0.1, 0.9), #k3
                            nrow = 3, ncol = 3))

policy      <- RandomPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 200L, simulations = 200L)

history     <- simulation$run()

Plot$new()$set_external(T, 11, 6L)$grid(history)

before <- history$get_data_table()

history$save_data("test.RData")

######################## playing around with logs ##############################



log_S     <- History$new()
log_S$load_data("test.RData")

bandit      <- LiLogBandit$new(log_S,3,3)
policy      <- LinUCBPolicy$new(1.0)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 200L, simulations = 200L)


history     <- simulation$run()

after <- history$get_data_table()

Plot$new()$set_external(T, 11, 6L)$grid(history)
