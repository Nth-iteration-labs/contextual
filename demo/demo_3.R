setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 100L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.5, 0.1,
                                  0.1, 0.1, 0.1), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = context_weights, precaching = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.1, R=0.1, epsilon=0.1), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

plot(history, type = "grid")
