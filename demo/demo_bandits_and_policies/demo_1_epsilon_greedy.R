library(contextual)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit             <- SyntheticBandit$new(weights = c(0.9, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)
history            <- Simulator$new(agents = agent, horizon = 100, simulations = 100, do_parallel = FALSE)$run()

summary(history)

plot(history, type = "cumulative", regret = TRUE)
h <- history$get_data_table()
cum_reward <- history$cumulative()
str(cum_reward)


library(contextual)

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit             <- SyntheticBandit$new(weights = c(0.9, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)
history            <- Simulator$new(agents = agent, horizon = 10,
                                    simulations = 10,
                                    do_parallel = FALSE,
                                    save_context = TRUE,
                                    save_theta = TRUE)$run()

print(history)

h <- history$get_data_table()

str(h, max.level = 1)
