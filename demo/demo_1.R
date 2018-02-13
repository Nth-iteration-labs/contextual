setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 100L
weight_per_arm     <- c(0.9, 0.1, 0.1)

policy             <- EpsilonFirstPolicy$new(first = 50, name = "EpsilonFirst")
bandit             <- SyntheticBandit$new(data = weight_per_arm, precache = FALSE)
agent              <- Agent$new(policy, bandit)

history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()

plot(history, type = "grid")

