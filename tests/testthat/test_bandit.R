context("AbstractBandit")

test_that("AbstractBandit check", {

  bandit <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  reward = list()
  reward["reward"]  = 10
  reward["choice"] = 1
  reward["is_optimal"] = 0
  reward["oracle"] = 1
  reward["propensity"] = 0

  action = list()
  action$choice = 2
  action$optimal_choice = 2

  expect_error(bandit$get_context(1), ".*implement.")
  expect_error(bandit$get_reward(action,1), ".*implement.")
  expect_error(bandit$set_weights(1), ".*implement.")
  expect_error(bandit$generate_cache(1), ".*implement.")


})
