context("CMAB Policies")

test_that("ContextualThompsonSamplingPolicy simulation", {

  bandit      <- ContextualBasicBandit$new(k = 5, d = 5)
  expect_identical(typeof(bandit), "environment")

  horizon       <- 10L
  simulations   <- 10L

  policy        <- ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5)
  expect_identical(typeof(policy), "environment")

  agent         <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret_var,8.2666666667)

})
