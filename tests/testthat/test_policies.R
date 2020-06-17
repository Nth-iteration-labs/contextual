context("Policies")

test_that("ContextualLogitBTSPolicy simulation", {

  horizon       <- 20L
  simulations   <- 10L

  bandit        <- ContextualLinearBandit$new(k = 5, d = 5, binary_rewards = TRUE)

  agents        <-list(
    Agent$new(ContextualLogitBTSPolicy$new(), bandit)
  )

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$ContextualLogitBTS$cum_reward,  6.2,  tolerance = 0.01)
  expect_equal(history$cumulative$ContextualLogitBTS$cum_regret,  11.6, tolerance = 0.01)

})

test_that("Exp3Policy simulation", {

  horizon       <- 20L
  simulations   <- 20L
  weights       <- c(0.9, 0.1, 0.1)

  policy        <- Exp3Policy$new(gamma = 0.1)
  bandit        <- BasicBernoulliBandit$new(weights = weights)
  agent         <- Agent$new(policy, bandit)

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$Exp3$cum_reward,  7.5,  tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret,  10.8, tolerance = 0.01)

})

