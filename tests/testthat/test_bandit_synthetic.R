context("ContextualWeightBandit")

test_that("ContextualWeightBandit simulation", {
  bandit      <- ContextualWeightBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_true(bandit$precaching)

  bandit$set_weights(c(0.1, 0.9))
  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 1)

  policy      <- EpsilonGreedyPolicy$new()
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")


})
