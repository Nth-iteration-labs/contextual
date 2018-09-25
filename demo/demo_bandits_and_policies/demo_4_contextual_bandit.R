#library(contextual)
library(here)
setwd(here::here("demo","demo_bandits_and_policies"))
source("../dev.R")


horizon            <- 200L
simulations        <- 50L

bandit             <- ContextualLogitBandit$new(k = 10L, d = 5L)

agents             <- list(
                           Agent$new(LinUCBDisjointPolicy$new(0.1), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(), bandit)
                           #Agent$new(ContextualThompsonSamplingPolicy$new(), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, legend_position = "topleft",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE)
