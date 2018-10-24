library(contextual)

horizon            <- 100
simulations        <- 1000
weights            <- matrix(c(0.8, 0.2, 0.2), 1, 3)

policy             <- EpsilonFirstPolicy$new(epsilon = 0.5, N = horizon)
bandit             <- ContextualBernoulliBandit$new(weights = weights)

agent              <- Agent$new(policy,bandit)

simulator          <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations)

history            <- simulator$run()

par(mfrow = c(1, 2), mar = c(2, 4, 1, 1), cex=1.4)  #bottom, left, top, and right.
plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE)
plot(history, type = "arms", no_par = TRUE)
par(mfrow = c(1, 1))
