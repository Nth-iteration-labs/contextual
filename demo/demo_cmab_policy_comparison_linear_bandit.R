library(contextual)

horizon       <- 1000L
simulations   <- 1L

bandit        <- ContextualLinearBandit$new(k = 10, d = 10)

agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
               Agent$new(ContextualEpsilonGreedy$new(0.1), bandit, "cEGreedy"),
               Agent$new(ContextualLogitBTSPolicy$new(10), bandit, "LogitBTS"),
               Agent$new(LinUCBDisjointPolicy$new(0.6), bandit, "LinUCB"))

simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)

history        <- simulation$run()

plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")

plot(history, type = "cumulative", rate = TRUE,  legend_position = "topright")
