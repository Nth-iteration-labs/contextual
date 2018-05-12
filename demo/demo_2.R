setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 10L
simulations        <- 40L
weights            <- matrix(  c( 0.4, 0.1, 0.2,
                                  0.2, 0.3, 0.1,
                                  0.1, 0.2, 0.3), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = weights, precaching = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "EpsilonGreedy"), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

#plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "cumulative", regret = TRUE)
plot(history, type = "cumulative", regret = FALSE)

print(history)
summary(history)
