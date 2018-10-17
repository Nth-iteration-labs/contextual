library(contextual)

bandit    <- BasicBernoulliBandit$new(weights = c(0.9, 0.1, 0.1))
policy    <- EpsilonGreedyPolicy$new(0.1)
agent     <- Agent$new(policy,bandit)
simulator <- Simulator$new(agents = agent, simulations = 10000, horizon = 40)
history   <- simulator$run()

summary(history)

par(mfrow = c(1, 1), mar = c(4, 4, 0.5, 1), cex=1.3)
plot(history, type = "arms", no_par = TRUE)
