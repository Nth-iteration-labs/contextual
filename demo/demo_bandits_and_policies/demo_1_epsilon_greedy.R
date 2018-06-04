library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)

policy             <- UCB1Policy$new()

bandit             <- SyntheticBandit$new(weights = c(0.9, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)
history            <- Simulator$new(agents = agent, horizon = 10, simulations = 100,
                                    do_parallel = TRUE)$run()

summary(history)

#plot(history, type = "cumulative", regret = TRUE, ci = TRUE)
#plot(history, type = "average", regret = TRUE, ci = TRUE)

h <- history$get_data_table()



#cum_reward <- history$cumulative()
#str(cum_reward)
