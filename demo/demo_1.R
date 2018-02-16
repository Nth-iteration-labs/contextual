setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100
simulations        <- 100
weight_per_arm     <- c(0.9, 0.1, 0.1)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1, name = "EG")
bandit             <- SyntheticBandit$new(data = weight_per_arm, precache = TRUE)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations, do_parallel = TRUE)$run()

plot(history, type = "grid")
plot(history, type = "arms")
