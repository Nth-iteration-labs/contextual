library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon            <- 3000L
simulations        <- 3L
context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                  0.1, 0.2, 0.1,
                                  0.1, 0.2, 0.1,
                                  0.1, 0.1, 0.1),  nrow = 4, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = context_weights, precaching = TRUE)

agents             <- list( Agent$new(OraclePolicy$new(), bandit),
                            Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                            Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit),
                            Agent$new(LinUCBDisjointSmPolicy$new(0.6), bandit),
                            Agent$new(RandomPolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative")
