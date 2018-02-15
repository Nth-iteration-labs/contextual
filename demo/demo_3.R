setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 10L
simulations        <- 10L
weights            <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3)

bandit             <- SyntheticBandit$new(data = weights)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

h <- history$get_data_table()

plot(history, type = "grid")
