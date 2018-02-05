setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon     <- 100L
simulations <- 300L
weights     <- c( 0.9, 0.1, 0.1 )

bandit      <- SyntheticBandit$new(reward_family = "Bernoulli", data = weights)
policy      <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")
agent       <- Agent$new(policy, bandit)

simulation  <- Simulator$new(agent, horizon, simulations)
history     <- simulation$run()

plot(history, type = "grid", ci = TRUE)
