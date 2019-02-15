context("Policy")

test_that("Policy", {

  policy      <- Policy$new()
  expect_identical(typeof(policy), "environment")

  policy$theta_to_arms <- list(n=3)
  theta <- policy$initialize_theta(4)
  expect_identical(theta$n[[4]], 3)

  expect_identical(policy$class_name, "Policy")
  expect_error(policy$get_action(), "has not been implemented")
  expect_error(policy$set_reward(), "has not been implemented")

})
