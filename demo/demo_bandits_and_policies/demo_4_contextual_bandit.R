#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

library(here)
setwd(here("demo","demo_bandits_and_policies"))

horizon            <- 3000L  # TODO: print all options..
simulations        <- 1L

# todo: default print from bandit...

##############  Generate general context plus users

bandit             <- ContextualBandit$new(k = 5, d = 6, num_users = 10)

agents             <- list(
                           Agent$new(LinUCBHybridSmPolicy$new(0.7), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(LinUCBDisjointSmPolicy$new(0.7), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()


plot(history, type = "cumulative", regret = FALSE, legend_position = "bottomright",  #ci = "ci",
              rate = TRUE) #traces = TRUE, smooth = TRUE)

print(history$meta$sim_total_duration)

##############  Generate general context

# bandit             <- ContextualBandit$new(k = 5, d = 6)
#
# agents             <- list(Agent$new(ContextualEpsilonGreedyPolicy$new(0.01), bandit),
#                            Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit),
#                            Agent$new(EpsilonGreedyPolicy$new(), bandit),
#                            Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
#                            Agent$new(LinUCBDisjointPolicy$new(0.7), bandit))
#
# simulation         <- Simulator$new(agents, horizon, simulations)
# history            <- simulation$run()
#
# plot(history, type = "cumulative", regret = FALSE, rate = TRUE)

