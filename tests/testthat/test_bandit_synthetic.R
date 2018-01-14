context("SyntheticBandit")

## add precache = FALSE and much more..

test_that("SyntheticBandit simulation", {
  bandit      <- SyntheticBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_true(bandit$is_precaching)
  expect_identical(bandit$get_weights(), matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))

  bandit$set_weights(c(0.1, 0.9))
  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 1)
  expect_identical(bandit$get_weights(), matrix(c(0.1, 0.9), 1, 2))

  policy      <- EpsilonGreedyPolicy$new()
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation  <-
    Simulator$new(
      agent,
      horizon = 2L,
      simulations = 2L,
      worker_max = 1
    )

  expect_output(bandit$object_size(), ".bytes.*")

  history     <- simulation$run()
  expect_equal(sum(history$data$reward), 3)

  context <- bandit$get_context()
  expect_equal(context$k, 2)
  expect_equal(context$d, 1)
  expect_equal(context$X, 1)
  expect_identical(context$O, c(0.1, 0.9))

  generated_weights <- bandit$generate_weights(2, 2)
  generated_weights_expected <-
    matrix(c(0.2655087, 0.3721239, 0.5728534, 0.9082078), 2, 2)
  expect_equal(round(generated_weights, 7), generated_weights_expected)
  expect_equal(round(bandit$get_weights(), 7), generated_weights_expected)

  expect_message(bandit$generate_cache(1), "Precaching bandit")

  expect_output(simulation$object_size(), ".bytes.*")

})
