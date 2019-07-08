library(contextual)

horizon     <- 100L
simulations <- 1000L

                      # k=1  k=2  k=3             -> columns represent arms
weights     <- matrix(c(0.6, 0.2, 0.2,     # d=1  -> rows represent
                        0.2, 0.6, 0.2,     # d=2     context features,
                        0.2, 0.2, 0.6),    # d=3

                      nrow = 3, ncol = 3, byrow = TRUE)

bandit      <- ContextualBernoulliBandit$new(weights = weights)

eg_policy   <- EpsilonGreedyPolicy$new(0.1)
lucb_policy <- LinUCBDisjointPolicy$new(0.6)

agents      <- list(Agent$new(eg_policy, bandit, "EGreedy"),
                    Agent$new(lucb_policy, bandit, "LinUCB"))

simulation  <- Simulator$new(agents, horizon, simulations, save_context = TRUE)
history     <- simulation$run()

par(mfrow = c(2, 3), mar = c(2, 4, 1, 0.1), cex=1.3)  #bottom, left, top, and right.

plot(history, type = "cumulative", legend_border = FALSE, no_par = TRUE )
plot(history, type = "arms",  limit_agents = c("LinUCB"), no_par = TRUE)
plot(history, type = "arms",  limit_agents = c("EGreedy"), no_par = TRUE)

plot(history, type = "arms",  limit_agents = c("LinUCB"), limit_context = c("X.1"), no_par = TRUE)
plot(history, type = "arms",  limit_agents = c("LinUCB"), limit_context = c("X.2"), no_par = TRUE )
plot(history, type = "arms",  limit_agents = c("LinUCB"), limit_context = c("X.3"), no_par = TRUE )

par(mfrow = c(1, 1))
