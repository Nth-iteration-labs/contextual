#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### create a random log ################################

bandit      <- SyntheticBandit$new()
bandit$set_weights(matrix(
  c(0.9, 0.0, 0.1,
    0.1, 0.9, 0.1,
    0.1, 0.1, 0.9),
  nrow = 3,
  ncol = 3
))

policy      <- RandomPolicy$new()

agent       <- Agent$new(policy, bandit)

simulation  <-
  Simulator$new(
    agent,
    horizon = 100L,
    simulations = 100L,
    save_context = TRUE
  )

before <- simulation$run()

before$save_data("test.RData")

plot(before, type = "grid")

######################## use the log to test a policy ##########################

log_S     <- History$new()
log_S$load_data("test.RData")

bandit      <- LiLogBandit$new(log_S, 3, 3)

policy      <- LinUCBPolicy$new(1.0)
agent       <- Agent$new(policy, bandit)
simulation  <- Simulator$new(agent, horizon = 100L, simulations = 100L, continouous_counter = TRUE )

after <- simulation$run()

plot(after, type = "grid")

if (file.exists("test.RData")) file.remove("test.RData")
