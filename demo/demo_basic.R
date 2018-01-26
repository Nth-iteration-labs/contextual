########################### package dev helpers ################################
#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

################################## timer #######################################

ptm <- proc.time()

########################### package dev helpers ################################

#bandit      <- SyntheticBandit$new(reward_family = "Gaussian", precache = F)   # set model here, and data

bandit      <- SyntheticBandit$new(reward_family = "Boolean", precache = F)   # set model here, and data


bandit$set_weights(c(0.9, 0.1, 0.1))

#policy      <- EpsilonGreedyPolicy$new(0.05)
policy     <- ThompsonSamplingPolicy$new(1.0,1.0)


agent       <- Agent$new(policy, bandit)

simulation  <- Simulator$new(
                              agent,
                              horizon     = 300L,
                              simulations = 100L,
                              save_context = TRUE,
                              save_theta = TRUE,
                              do_parallel = F
                            )

history     <- simulation$run()

history_df  <- history$get_data_frame()

Plot$new()$grid(history)

################################## timer #######################################


print(proc.time() - ptm)
