setwd("~/GitHub/contextual/demo")
source("dev.R")

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit             <- SyntheticBandit$new(weights = c(0.9, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)
history            <- Simulator$new(agents = agent, horizon = 100, simulations = 100, do_parallel = FALSE, continuous_counter = TRUE)$run()

summary(history)


plot(history, type = "cumulative", regret = TRUE)
h <- history$get_data_table()
cum_reward <- history$cumulative()
str(cum_reward)
