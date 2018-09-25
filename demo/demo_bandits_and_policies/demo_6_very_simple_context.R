library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")
#
# horizon       <- 5000L
# simulations   <- 5L
#
# bandit        <- ContextualLogitBandit$new(k = 5, d = 10, intercept = TRUE)
#
# agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
#                       Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#                       Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
#                       Agent$new(ContextualEpochGreedyPolicy$new(8), bandit))
#
# simulation     <- Simulator$new(agents, horizon, simulations)
#
# history        <- simulation$run()
#
# plot(history, type = "cumulative", rate = TRUE, regret = FALSE, legend_position = "bottomright", smooth = TRUE)
# plot(history, type = "cumulative", rate = FALSE, regret = TRUE, legend_position = "bottomright", smooth = TRUE)


##########
#bandit        <- ContextualHybridBandit$new(k = 100, shared_features = 10, unique_features = 2)
##########

horizon       <- 1000L
simulations   <- 10L

bandit        <- ContextualLinearBandit$new(k = 5, d = 5)

agents        <-list(#Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                     Agent$new(ContextualLogitBTS$new(), bandit)
                     #Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                     #Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit)
                     )

simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history        <- simulation$run()
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", smooth = T)



################### mostly unique

# bandit         <- ContextualGeneratorBandit$new(k = 100, s = 2, u = 10)
#
# agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
#                       Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#                       Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
#                       Agent$new(ContextualEpochGreedyPolicy$new(8), bandit),
#                       Agent$new(LinUCBHybridOptimizedPolicy$new(0.6), bandit),
#                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#
# simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
# history        <- simulation$run()
#
# plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", smooth = T)
