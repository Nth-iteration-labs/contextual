library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

########################### run bandit directly ################################

context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                  0.1, 0.8, 0.1,
                                  0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)
horizon     <- 600L
simulations <- 20L
bandit      <- ContextualBernoulliBandit$new(weights = context_weights, sum_weights = TRUE)

# This can only be random policy, otherwise rejection sampling will
# produce severely biased results.

policy      <- RandomPolicy$new()

agents <-
  list(
    Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
    Agent$new(LinUCBDisjointPolicy$new(0.6), bandit),
    Agent$new(OraclePolicy$new(), bandit)
  )

simulation  <-
  Simulator$new(
    agents,
    horizon = horizon,
    simulations = simulations,
    save_context = TRUE
  )

direct <- simulation$run()
plot(direct, regret = FALSE, type = "cumulative", rate = TRUE, legend_position = "bottomright",ylim = c(0.5,0.95))

########################### create random log data ################################

context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
horizon     <- 2000L
simulations <- 10L
bandit      <- ContextualBernoulliBandit$new(weights = context_weights, sum_weights = TRUE)

# Has to be a random policy, otherwise rejection sampling will
# produce biased results.

policy      <- RandomPolicy$new()

agent       <- Agent$new(policy, bandit)

simulation  <-
  Simulator$new(
    agent,
    horizon = horizon,
    simulations = simulations,
    save_context = TRUE
  )

random_stream <- simulation$run()
random_stream$save("test.RData")

######################## use the log to test a policy ##########################

history <- History$new()
history$load("test.RData")
log_S <- history$get_data_table()

bandit <- OfflinePolicyEvaluatorBandit$new(data_stream = log_S, k = 3, d = 3)

agents <-
  list(
    Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
    Agent$new(LinUCBDisjointPolicy$new(0.6), bandit),
  )

simulation <-
  Simulator$new(
    agents,
    horizon = horizon,
    simulations = simulations,
    t_over_sims = TRUE,
    do_parallel = FALSE,
    reindex = TRUE
  )

after <- simulation$run()
dt <- after$get_data_table()


plot(after, regret = FALSE, type = "cumulative", rate = TRUE, ylim = c(0.5,0.95),legend_position = "bottomright")
a <- after$get_data_table()
if (file.exists("test.RData")) file.remove("test.RData")
