setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 300L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(context_weights = context_weights, precache = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

plot(history, type = "grid")
