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
