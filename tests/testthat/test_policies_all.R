context("Policies")

test_that("Exp3", {


  arm_weights <- c(0.1, 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights  = arm_weights)
  expect_identical(typeof(bandit), "environment")

  policy      <- Exp3Policy$new(0.1, "Exp3")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 48)
})

test_that("Oracle", {
  arm_weights <- c(0.1, 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights  = arm_weights)
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- OraclePolicy$new("Oracle")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 92)
})

test_that("Test Random", {
  arm_weights <- c(0.1, 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights  = arm_weights)
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- RandomPolicy$new("Random")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 42)
})

test_that("ThompsonSampling", {

  arm_weights <- c(0.1, 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights  = arm_weights)

  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 71)
})

test_that("EpsilonGreedy", {
  arm_weights <- c(0.1, 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights  = arm_weights)
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 55)
})








