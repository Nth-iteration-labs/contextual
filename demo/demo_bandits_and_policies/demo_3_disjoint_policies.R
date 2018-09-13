library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon            <- 100L
simulations        <- 100L
context_weights    <- matrix(  c(0.4, 0.2, 0.4,
                                 0.3, 0.4, 0.3,
                                 0.1, 0.8, 0.1),  nrow = 3, ncol = 3, byrow = TRUE)

context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- ContextualWeightBandit$new(weights = context_weights, precaching = FALSE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                            Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

plot(history, type = "cumulative")

plot(history, type = "cumulative", regret = FALSE, rate =TRUE)
