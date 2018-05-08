#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

########################### create a random log ################################

context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.9, 0.1, 0.1), nrow = 2, ncol = 3, byrow = TRUE)
horizon     <- 100L
simulations <- 100L
bandit      <- SyntheticBandit$new(weights = context_weights)
policy      <- EpsilonGreedyPolicy$new()
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
#plot(before, type = "cumulative")

b <- before$get_data_table()


######################## use the log to test a policy ##########################

log_S <- History$new()
log_S$load_data("test.RData")

bandit <- DoublyRobustOfflineBandit$new(data_file = log_S, k = 3, d = 2)
#bandit <- LiSamplingOfflineBandit$new(data_file = log_S, k = 3, d = 2)

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
    do_parallel = FALSE,
    save_context = TRUE
  )

after <- simulation$run()
plot(after, type = "cumulative")

a <- after$get_data_table()

if (file.exists("test.RData")) file.remove("test.RData")
