#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

library(here)
setwd(here("demo","demo_bandits_and_policies"))

horizon            <- 500L
simulations        <- 50L

bandit             <- BasicContextualBandit$new(k = 3L, d = 6)

agents             <- list(
                           Agent$new(LinUCBDisjointPolicy$new(0.1), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualEpochGreedyDisjointPolicy$new(20), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(v=0.1), bandit),
                           Agent$new(ContextualDisjointThompsonSamplingPolicy$new(v=0.1), bandit),
                           Agent$new(RandomPolicy$new(), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, legend_position = "topleft",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE) # todo: false
