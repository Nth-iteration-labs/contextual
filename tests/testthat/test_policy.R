context("Policy")

test_that("Policy check", {

  policy <- Policy$new()
  expect_identical(typeof(policy), "environment")

  expect_error(policy$get_action(1,2), ".*implement.")
  expect_error(policy$set_reward(1,2), ".*implement.")
  expect_error(policy$set_parameters(), ".*implement.")

})
