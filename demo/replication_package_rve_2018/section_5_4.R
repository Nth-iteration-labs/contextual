library(contextual)

horizon <- 100L
simulations <- 300L

weights <- matrix(c(0.8, 0.1, 0.1,
                    0.1, 0.8, 0.1,
                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)

bandit <- ContextualBernoulliBandit$new(weights = weights, precaching = TRUE)
agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
               Agent$new(LinUCBDisjointPolicy$new(0.6), bandit, "LinUCB"))
simulation <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history <- simulation$run()

par(mfrow = c(1, 3), mar = c(2, 4, 0.5, 1), cex=1.5)
plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
plot(history, type = "arms",  limit_agents = c("EGreedy"), no_par = TRUE)
plot(history, type = "arms",  limit_agents = c("LinUCB"), no_par = TRUE)
par(mfrow = c(1, 1))
