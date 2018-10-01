library(contextual)

horizon            <- 100
simulations        <- 100
weights            <- c(0.6, 0.3, 0.3)

policy             <- EpsilonFirstPolicy$new(epsilon = 0.5, N = horizon)
bandit             <- ContextualBernoulliBandit$new(weights = weights)

agent              <- Agent$new(policy,bandit)

simulator          <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations)

history            <- simulator$run()

plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE)
plot(history, type = "arms", no_par = TRUE)
