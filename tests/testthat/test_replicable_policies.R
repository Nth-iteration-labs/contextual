context("Policies")

test_that("Test Exp3", {


  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))

  expect_equal(bandit$k, 3)
  expect_equal(bandit$d, 1)
  expect_false(bandit$is_precaching)
  expect_warning(bandit$is_precaching <- TRUE,".*locked*")
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- Exp3Policy$new(0.1, "Exp3")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)

  expect_error(bandit$generate_cache(1),".*precaching.*")

  context <- bandit$get_context()
  expect_equal(context$k, 3)
  expect_equal(context$d, 1)
  expect_equal(context$X, 1)
  expect_identical(context$O, c(0.1,0.9,0.1))

  history     <- simulation$run()
  expect_equal(sum(history$reward), 45)

})

test_that("Test Oracle", {


  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))

  expect_equal(bandit$k, 3)
  expect_equal(bandit$d, 1)
  expect_false(bandit$is_precaching)
  expect_warning(bandit$is_precaching <- TRUE,".*locked*")
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- OraclePolicy$new("Oracle")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)

  expect_error(bandit$generate_cache(1),".*precaching.*")

  context <- bandit$get_context()
  expect_equal(context$k, 3)
  expect_equal(context$d, 1)
  expect_equal(context$X, 1)
  expect_identical(context$O, c(0.1,0.9,0.1))

  history     <- simulation$run()
  expect_equal(sum(history$reward), 90)

})

test_that("Test Random", {

  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(c(0.1, 0.9, 0.1))

  expect_equal(bandit$k, 3)
  expect_equal(bandit$d, 1)
  expect_false(bandit$is_precaching)
  expect_warning(bandit$is_precaching <- TRUE,".*locked*")
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1),1,3))

  policy      <- RandomPolicy$new("Random")
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)

  expect_error(bandit$generate_cache(1),".*precaching.*")

  context <- bandit$get_context()
  expect_equal(context$k, 3)
  expect_equal(context$d, 1)
  expect_equal(context$X, 1)
  expect_identical(context$O, c(0.1,0.9,0.1))

  history     <- simulation$run()
  expect_equal(sum(history$reward), 36)

})





