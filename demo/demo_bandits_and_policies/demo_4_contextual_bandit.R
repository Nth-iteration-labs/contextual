#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

library(here)
setwd(here("demo","demo_bandits_and_policies"))

horizon            <- 2000L
simulations        <- 30L

bandit             <- BasicContextualBandit$new(k = 5L, common = 5L, unique = 5L)

agents             <- list(
                           Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualEpochGreedyDisjointPolicy$new(100), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()


plot(history, type = "cumulative", regret = FALSE, legend_position = "bottomright",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE) # todo: false
