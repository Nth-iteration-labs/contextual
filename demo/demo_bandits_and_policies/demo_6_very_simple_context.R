library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon         <- 3000L
simulations     <- 100L

bandit          <- ContextualGeneratorHybridBandit$new(k = 10, s = 1, u = 5)

agents          <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                        Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                        Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
                        Agent$new(ContextualEpochGreedyPolicy$new(8), bandit),
                        Agent$new(LinUCBHybridOptimizedPolicy$new(0.6), bandit),
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

simulation      <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history         <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, smooth = TRUE, legend_position = "bottomright")
