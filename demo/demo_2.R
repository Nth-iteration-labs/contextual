setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 10L
simulations        <- 10L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(context_weights = context_weights, precache = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative")
