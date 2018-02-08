setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 1000L
weight_per_arm    <- c( 0.9, 0.1, 0.1 )

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit             <- SyntheticBandit$new(data = weight_per_arm)
agent              <- Agent$new(policy, bandit)

simulation         <- Simulator$new(agent, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid", ci = TRUE)
