library(contextual)

bandit  <- ContextualBernoulliBandit$new(matrix(c(0.5, 0.2, 0.1), 1))
policy  <- EpsilonGreedyPolicy$new(0.1)
agent   <- Agent$new(policy,bandit)
sim     <- Simulator$new(agent, simulations = 10000, horizon = 100)
history <- sim$run()

summary(history)

par(mfrow = c(1, 1), mar = c(4, 4, 0.5, 1), cex=1.3)
plot(history, type = "arms", no_par = TRUE)
