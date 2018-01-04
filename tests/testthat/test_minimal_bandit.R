context("AbstractBandit")

test_that("Minimal bandit setup test", {

  bandit      <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(matrix(c(0.1,0.9,0.1,0.9),2,2))
  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_false(bandit$is_precaching)
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9,0.1,0.9),2,2))

  bandit$set_weights(c(0.1,0.9))
  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 1)
  expect_identical(bandit$get_weights(), matrix(c(0.1,0.9),1,2))

  policy      <- EpsilonGreedyPolicy$new()
  expect_identical(typeof(policy), "environment")



  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <- SimulatorParallel$new(agent, horizon = 2L, simulations = 2L, worker_max = 1)

  expect_error(bandit$generate_cache(1),".*precaching.*")
  expect_identical(bandit$object_size(), bandit$hash)

  context <- bandit$get_context()

  expect_equal(context$k, 2)
  expect_equal(context$d, 1)
  expect_equal(context$X, 1)
  expect_identical(context$O, c(0.1,0.9))

  history     <- simulation$run()
  expect_equal(sum(history$reward), 3)

})
