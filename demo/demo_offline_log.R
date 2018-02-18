#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### create a random log ################################

bandit      <- SyntheticBandit$new()
bandit$set_weights(c(0.9, 0.0, 0.1)
)

policy      <- EpsilonGreedyPolicy$new()

agent       <- Agent$new(policy, bandit)

simulation  <-
  Simulator$new(
    agent,
    horizon = 100L,
    simulations = 100L,
    save_context = TRUE
  )

before <- simulation$run()

b <- before$get_data_table()

before$save_data("test.RData")

plot(before, type = "grid")

######################## use the log to test a policy ##########################

log_S     <- History$new()
log_S$load_data("test.RData")

bandit      <- OfflineLiBandit$new(log_S, 3, 3)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit))

simulation  <- Simulator$new(agents, horizon = 100L, simulations = 100L, continouous_counter = TRUE, do_parallel = FALSE )

after <- simulation$run()

a <- after$get_data_table()

plot(after, type = "grid")

if (file.exists("test.RData")) file.remove("test.RData")
