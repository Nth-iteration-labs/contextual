library(contextual)
library(here)
setwd(here("demo","paper_jss_replication_package"))

horizon            <- 100
simulations        <- 1000
weights            <- c(0.6, 0.3, 0.3)

policy             <- EpsilonFirstPolicy$new(first = 50)
bandit             <- SyntheticBandit$new(weights = weights)

agent              <- Agent$new(policy,bandit)

simulator          <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations,
                                    do_parallel = FALSE)

history            <- simulator$run()

par(mfrow = c(1, 2), mar = c(2, 4, 0.5, 1), cex=1.5)
plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
plot(history, type = "arms", no_par = TRUE)
par(mfrow = c(1, 1))
