setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100
simulations        <- 100
weight_per_arm     <- c(0.9, 0.1, 0.1)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1, name = "EG")
bandit             <- SyntheticBandit$new(weights = weight_per_arm)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations,
                                    save_theta = TRUE,
                                    do_parallel = FALSE
                                    )$run()

plot(history, type = "grid")

h <- history$get_data_table()
