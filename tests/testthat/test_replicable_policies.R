context("Policies")

test_that("Test Exp3", {
  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- Exp3Policy$new(0.1, "Exp3")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 45)
})

test_that("Test Oracle", {
  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- OraclePolicy$new("Oracle")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 90)
})

test_that("Test Random", {
  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- RandomPolicy$new("Random")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 36)
})

test_that("Test LinUCB", {
  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- LinUCBPolicy$new(1.0, "LinUCB")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 74)
})

test_that("Test ThompsonSampling", {

  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 59)
})

test_that("Test EpsilonGreedy", {
  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()
  expect_equal(sum(history$reward), 37)
})








