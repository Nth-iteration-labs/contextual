setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 5000L
weights            <- matrix(  c( 0.9, 0.3, 0.2,
                                  0.5, 0.6, 0.2,
                                  0.2, 0.1, 0.5),  nrow = 3, ncol = 3  )

bandit             <- SyntheticBandit$new(data = weights)

agents             <- list( Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1,1, "TSampling"), bandit),
                            Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid")
