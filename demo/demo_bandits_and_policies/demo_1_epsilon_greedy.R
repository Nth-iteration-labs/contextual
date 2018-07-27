library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

#library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))

policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)

bandit             <- SyntheticBandit$new(weights = c(0.6, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)

history            <-  Simulator$new(
                          agents = agent,
                          horizon = 100,
                          simulations = 100,
                          do_parallel = FALSE,
                          progress_file = FALSE
                        )$run()

print(history$meta$sim_total_duration)
print(history$meta$sim_end_time)

plot(history, type = "average", regret = TRUE, ci = "ci", smooth = FALSE, interval = 1)
plot(history, type = "cumulative", regret = TRUE, ci = "ci",  traces_max = 100, traces_alpha = 0.1,
     traces = TRUE, smooth = FALSE, interval = 1)

summary(history)
h <- history$get_data_table()
