context("Bandit")

test_that("Bandit test.", {

  bandit <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  expect_error(bandit$get_context(1), ".*implement.")
  expect_error(bandit$get_reward(1), ".*implement.")
  expect_error(bandit$set_weights(1), ".*implement.")
  expect_error(bandit$generate_cache(1), ".*implement.")

})
