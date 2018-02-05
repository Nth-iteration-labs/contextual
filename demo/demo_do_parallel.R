setwd("~/GitHub/contextual/demo")
source("dev.R")

bandit <- SyntheticBandit$new(reward_family = "Bernoulli", seed = 1, precache = TRUE)

                             #k1   #k2   #k3
bandit$set_weights(matrix(c( 0.9,  0.1,  0.1,
                             0.1,  0.2,  0.1,
                             0.2,  0.1,  0.2 ), nrow = 3L, ncol = 3L ))

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

simulation     <- Simulator$new(
  agents,
  horizon      = 100L,
  simulations  = 100L,
  save_context = FALSE,
  save_theta   = FALSE,
  do_parallel  = TRUE
)

history <- simulation$run()

plot(history, type = "grid")
