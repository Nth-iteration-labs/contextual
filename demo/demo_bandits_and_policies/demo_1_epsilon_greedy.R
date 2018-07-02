library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
policy             <- UCB1Policy$new()

bandit             <- SyntheticBandit$new(weights = c(0.6, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)
history            <- Simulator$new(agents = agent, horizon = 100, simulations = 300)$run()

print(history$meta$sim_total_duration)

plot(history, type = "average", regret = TRUE, ci = "ci", smooth = FALSE, step_size = 1)
plot(history, type = "cumulative", regret = TRUE, ci = "ci",
     traces_max = 100, traces_alpha = 0.1, traces = TRUE, smooth = FALSE, step_size = 1)

summary(history)
h <- history$get_data_table()
cum_reward <- history$cumulative
