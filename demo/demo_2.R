setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 1000L
weights            <- matrix(  c( 0.9, 0.3, 0.2,
                                  0.3, 0.5, 0.3,
                                  0.2, 0.2, 0.1),  nrow = 3, ncol = 3)

bandit             <- SyntheticBandit$new(data = weights )

agents             <- list( Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1, 1, "TS"), bandit),
                            Agent$new(EpsilonGreedyPolicy$new(0.1, "TS"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid")
