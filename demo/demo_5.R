setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 100L
weight_per_arm     <- c( 0.9, 0.1, 0.1)

bandit             <- SyntheticBandit$new(data = weight_per_arm)
agents             <- list(
                             Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit)
                             #Agent$new(EpsilonFirstPolicy$new(20, "\U190-first"), bandit)
                             #Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
                           )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "arms") ############ better error message

