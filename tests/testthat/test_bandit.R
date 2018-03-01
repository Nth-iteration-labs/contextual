context("AbstractBandit")

test_that("AbstractBandit check", {

  bandit <- AbstractBandit$new()
  expect_identical(typeof(bandit), "environment")

  reward = list()
  reward["reward"]  = 10
  reward["is_optimal"] = 0
  reward["oracle"] = 1

  action = list()
  action$choice = 2

  expect_error(bandit$get_context(1), ".*implement.")
  expect_error(bandit$do_action(action,1), ".*implement.")
  expect_error(bandit$set_weights(1), ".*implement.")
  expect_error(bandit$generate_bandit_data(1), ".*implement.")


})
