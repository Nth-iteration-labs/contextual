#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### create a random log ################################

context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
horizon     <- 10L
simulations <- 10L
bandit      <- SyntheticBandit$new(weights = context_weights)

# This can only be random policy, otherwise rejection sampling will
# produce severely biased results.

policy      <- RandomPolicy$new()

agent       <- Agent$new(policy, bandit)

simulation  <-
  Simulator$new(
    agent,
    horizon = horizon,
    simulations = simulations,
    save_context = TRUE
  )

before <- simulation$run()
before$save_data("test.RData")
plot(before, type = "grid")

b <- before$get_data_table()

######################## use the log to test a policy ##########################

log_S <- History$new()
log_S$load_data("test.RData")

bandit <- RejectionSamplingOfflineBandit$new(data_file = log_S, k = 3, d = 3)

agents <-
  list(
    Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
    Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit)
  )

simulation <-
  Simulator$new(
    agents,
    horizon = horizon,
    simulations = simulations,
    continouous_counter = TRUE,
    do_parallel = FALSE
  )

after <- simulation$run()
plot(after, type = "grid")
a <- after$get_data_table()
if (file.exists("test.RData")) file.remove("test.RData")
