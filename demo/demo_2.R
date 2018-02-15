setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 100L
weight_per_arm     <- c(0.9, 0.1, 0.1)

bandit             <- SyntheticBandit$new(data = weight_per_arm)
agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0, "TS"), bandit),
                            Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, save_context = TRUE, save_theta = TRUE)
history            <- simulation$run()

plot(history, type = "grid")

h <- history$get_data_table()
