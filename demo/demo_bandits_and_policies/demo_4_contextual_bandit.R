library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon            <- 1000L
simulations        <- 10L



##############  Generate general context plus users

bandit             <- ContextualBandit$new(k = 5, d = 6, num_users = 7)

agents             <- list(Agent$new(LinUCBHybridPolicy$new(0.7, 6), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(LinUCBDisjointPolicy$new(0.7), bandit))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative", ci = "ci", regret = FALSE,
              rate = TRUE, traces = TRUE, smooth = TRUE)

##############  Generate general context

bandit             <- ContextualBandit$new(k = 5, d = 6)

agents             <- list(Agent$new(ContextualEpsilonGreedyPolicy$new(0.01), bandit),
                           Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(LinUCBDisjointPolicy$new(0.7), bandit))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE)

