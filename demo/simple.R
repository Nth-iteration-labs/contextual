setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 2000L
simulations        <- 100L
context_weights    <- matrix(  c( 0.4, 0.15,
                                  0.4, 0.6),  nrow = 2, ncol = 2, byrow = TRUE )

policy             <- SimpleBTSPolicy$new()
bandit             <- SyntheticBandit$new(context_weights = context_weights, precache = TRUE, random_one_feature = TRUE)

agent              <- Agent$new(policy, bandit)

history            <- Simulator$new(agent, horizon, simulations, save_theta = TRUE, save_context = TRUE, do_parallel = FALSE)$run()


plot(history, type = "average", ci = TRUE,  start_step = 80, step_size = 50)

plot(history, type = "cumulative", ci = TRUE, rate = TRUE, start_step = 80, regret = FALSE)

#plot(history, type = "arms")
