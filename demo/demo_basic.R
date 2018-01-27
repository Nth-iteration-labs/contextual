########################### package dev helpers ################################
#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

################################## timer #######################################

ptm <- proc.time()

########################### package dev helpers ################################


data        <- c(0.9, 0.1, 0.1)

bandit      <- SyntheticBandit$new(reward_family = "Bernoulli", data = data)

policy      <- EpsilonGreedyPolicy$new(0.05)

agent       <- Agent$new(policy, bandit)

simulation  <- Simulator$new(
                              agent,
                              horizon      = 100L,
                              simulations  = 100L,
                              save_context = FALSE,
                              save_theta   = FALSE,
                              do_parallel  = F
                            )

history     <- simulation$run()

history_df  <- history$get_data_frame()

Plot$new()$grid(history)

################################## timer #######################################

print(proc.time() - ptm)


abstractbandit = AbstractBandit$new()
