library(contextual)

policy    <- ThompsonSamplingPolicy$new()
bandit    <- BasicBernoulliBandit$new(weights = c(0.9, 0.1, 0.1))
agent     <- Agent$new(policy,bandit)
simulator <- Simulator$new(agents = agent, simulations = 100, horizon = 50)
history   <- simulator$run()

summary(history)

plot(history, type = "arms")
