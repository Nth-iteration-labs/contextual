#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

library(here)
setwd(here("demo","demo_bandits_and_policies"))

horizon            <- 5000L
simulations        <- 5L

bandit             <- ContextualBandit$new(k = 50L, n_disjoint = 5, n_shared = 6)

agents             <- list(
                           Agent$new(LinUCBHybridOptimizedPolicy$new(0.1), bandit),
                           Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualEpochGreedyDisjointPolicy$new(100), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()


plot(history, type = "cumulative", regret = FALSE, legend_position = "bottomright",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE) # todo: false
