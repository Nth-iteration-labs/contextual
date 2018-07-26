#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

library(here)
setwd(here("demo","demo_bandits_and_policies"))

horizon            <- 20000L
simulations        <- 1L

##############  Generate general context plus users

bandit             <- ContextualBandit$new(k = 5, d = 6, num_users = 11)

agents             <- list(
                           Agent$new(LinUCBHybridOptimizedPolicy$new(0.7), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualEpochGreedyDisjointPolicy$new(300), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(LinUCBDisjointOptimizedPolicy$new(0.7), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()


plot(history, type = "cumulative", regret = FALSE, legend_position = "bottomright",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE)

##############  Generate general context

bandit             <- ContextualBandit$new(k = 5, d = 6)

agents             <- list(
                            Agent$new(LinUCBHybridOptimizedPolicy$new(0.7), bandit),
                            Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                            Agent$new(ContextualEpochGreedyDisjointPolicy$new(300), bandit),
                            Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                            Agent$new(LinUCBDisjointOptimizedPolicy$new(0.7), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE)

