setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 2000L
simulations        <- 100L
weight_per_arm     <- matrix(  c( 0.4, 0.15,
                                  0.4, 0.6),  nrow = 2, ncol = 2  )

policy             <- SimpleBTSPolicy$new()
bandit             <- SyntheticBandit$new(weights = weight_per_arm, precache = TRUE, random_one_feature = TRUE)

agent              <- Agent$new(policy, bandit)

history            <- Simulator$new(agent, horizon, simulations, save_theta = TRUE, save_context = TRUE)$run()

plot(history, type = "grid", ci = TRUE, rate = TRUE, start_step = 110, step_size = 50)

h <- history$get_data_table()

