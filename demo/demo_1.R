setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100
simulations        <- 30
arm_weights        <- c(0.9, 0.1, 0.1)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1, name = "EG")
bandit             <- SyntheticBandit$new(arm_weights = arm_weights)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations,
                                    save_theta = TRUE,
                                    do_parallel = FALSE)$run()

plot(history, type = "grid")
plot(history, type = "arms")

h <- history$get_data_table()





