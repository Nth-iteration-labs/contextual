########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

################################################################################

set.seed(21)

ptm <- proc.time()

AbstractBandit$set("public", "get_reward",
                   function(action, t) {
                     self$calculate_reward(runif(self$k) < self$get_weights())
                     self$do_reward(action)
                   }, overwrite = TRUE)

bandit <- AbstractBandit$new()
bandit$set_weights(c(0.1, 0.1, 0.9))

policy      <- EpsilonGreedyPolicy$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

print(proc.time() - ptm)

Plot$new()$set_external(T, 11, 6L)$grid(history)
