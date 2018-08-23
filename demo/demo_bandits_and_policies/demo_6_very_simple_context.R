library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon         <- 1000L
simulations     <- 1L
context_weights <- matrix(  c( 0.8, 0.3, 0.2,
                                  0.1, 0.6, 0.7),  nrow = 2, ncol = 3, byrow = TRUE)

bandit          <- VeryBasicContextualBandit$new(5,5)

agents          <- list( Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                         Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                         Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

simulation      <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history         <- simulation$run()

plot(history, type = "cumulative")
