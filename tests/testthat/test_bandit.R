context("Bandit")

test_that("Bandit test.", {

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

  context_list <- bandit$context_to_list()     ### shouldn't this be more informative, that is, state that has to be implemented etc..
  expect_equal(context_list$k,NULL)
  expect_equal(context_list$d,NULL)
  expect_equal(context_list$X,1)
  expect_equal(context_list$O,c(0,0,0))

  reward_list <- bandit$reward_to_list(action,1)
  expect_equal(reward_list$reward,0)
  #expect_true(!unlist(reward_list$is_optimal))
  expect_equal(reward_list$oracle,0)
  expect_equal(reward_list$propensity,NULL)


})
