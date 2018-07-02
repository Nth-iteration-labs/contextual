library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

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
plot(before, type = "cumulative")

b <- before$get_data_table()

######################## use the log to test a policy ##########################

log_S <- History$new()
log_S$load_data("test.RData")

bandit <- LiSamplingOfflineBandit$new(data_stream = log_S, k = 3, d = 3)

agents <-
  list(
    Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
    Agent$new(LinUCBDisjointPolicy$new(1.0), bandit)
  )

simulation <-
  Simulator$new(
    agents,
    horizon = horizon,
    simulations = simulations,
    continuous_counter = TRUE,
    do_parallel = FALSE
  )

after <- simulation$run()
plot(after, type = "cumulative")
a <- after$get_data_table()
if (file.exists("test.RData")) file.remove("test.RData")
