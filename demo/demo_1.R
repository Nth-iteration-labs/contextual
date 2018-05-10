setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100
simulations        <- 100
arm_weights        <- c(0.9, 0.1, 0.1)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1, name = "EG")
bandit             <- SyntheticBandit$new(weights = arm_weights, precaching = TRUE)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(agents = agent,
                                    horizon = horizon,
                                    simulations = simulations,
                                    save_theta = FALSE,
                                    do_parallel = TRUE)$run()

plot(history, type = "arms")
plot(history, type = "cumulative", regret = FALSE)
h <- history$get_data_table()
cum_reward <- history$cumulative()
print(cum_reward)
