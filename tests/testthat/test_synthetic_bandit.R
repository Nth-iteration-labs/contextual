context("SyntheticBandit")

test_that("SyntheticBandit simulation test.", {

  bandit      <- SyntheticBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(matrix(c(0.1,0.9,0.1,0.9),2,2))

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_true(bandit$is_precaching)
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

  ########expect_error(bandit$generate_cache(1),".*precaching.*")  ########## extend
  expect_identical(bandit$object_size(), bandit$hash)

  history     <- simulation$run()
  expect_equal(sum(history$reward), 3)

  context <- bandit$get_context()  ########### without n, than..  check
  expect_equal(context$k, 2)
  expect_equal(context$d, 1)
  expect_equal(context$X, rep(1,4))
  expect_equal(context$O, matrix(c(0.1,0.9,0.1,0.9,0.1,0.9,0.1,0.9),2,4))

})
