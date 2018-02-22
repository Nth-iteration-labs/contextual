setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 10000
simulations        <- 500
weights            <- matrix(  c( 0.9, 0.3, 0.2,
                                  0.5, 0.6, 0.2,
                                  0.2, 0.1, 0.5),  nrow = 3, ncol = 3  )

#weights            <- c(0.9, 0.1, 0.1)

bandit             <- SyntheticBandit$new(weights = weights)

agents             <- list( Agent$new(OraclePolicy$new("Oracle"), bandit),
                            #Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1,1, "TSampling"), bandit),
                            Agent$new(EpsilonGreedyPolicy$new(0.01, "\U190-greedy"), bandit),
                            Agent$new(LinUCBPolicy$new(), bandit)
                            )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid", ci = TRUE, step_size = 50, start_step = 100, rate = TRUE)

h <- history$get_data_table()
