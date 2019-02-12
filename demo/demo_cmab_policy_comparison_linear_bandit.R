library(contextual)

horizon       <- 400L
simulations   <- 300L

bandit        <- ContextualLinearBandit$new(k = 5, d = 5, sigma = 0.1)

agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
               Agent$new(ContextualEpsilonGreedyPolicy$new(0.1), bandit, "cEGreedy"),
               Agent$new(ContextualLinTSPolicy$new(0.01), bandit, "LinTS"),
               Agent$new(LinUCBDisjointOptimizedPolicy$new(1), bandit, "LinUCB"))

simulation     <- Simulator$new(agents, horizon, simulations)

history        <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "topleft")
