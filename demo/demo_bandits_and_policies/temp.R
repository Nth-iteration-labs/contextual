library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")



policy    <- EpsilonGreedyPolicy$new(epsilon = 0.1)
bandit    <- BasicBernoulliBandit$new(p_per_arm = c(0.9, 0.1, 0.1))
agent     <- Agent$new(policy,bandit)
simulator <- Simulator$new(agents = agent, simulations = 100, horizon = 80)
history   <- simulator$run()
