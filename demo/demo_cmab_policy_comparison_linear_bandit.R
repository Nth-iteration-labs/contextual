library(contextual)

horizon       <- 100L
simulations   <- 1L

bandit        <- ContextualLinearBandit$new(k = 5, d = 10, sigma = 1.0)

agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
               #Agent$new(ContextualEpsilonGreedyPolicy$new(0.1), bandit, "cEGreedy"),
               Agent$new(ContextualLinTSPolicy$new(0.01), bandit, "LinTS"),
               Agent$new(LinUCBDisjointOptimizedPolicy$new(1), bandit, "LinUCB"),
               Agent$new(ContextualLinACTSPolicy$new(0.1, 0.1, 0.9), bandit, "ACTS"))

simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)

history        <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, legend_position = "topleft")
