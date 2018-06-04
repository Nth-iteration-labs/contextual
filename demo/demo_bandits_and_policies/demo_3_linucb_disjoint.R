library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


horizon            <- 100L
simulations        <- 100L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.5, 0.1,
                                  0.1, 0.1, 0.1), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = context_weights, precaching = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

plot(history, type = "cumulative")
