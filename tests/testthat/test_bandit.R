context("Bandit")

test_that("Bandit simulation", {

  bandit      <- Bandit$new()
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$post_initialization()$class_name, "Bandit")
  expect_identical(bandit$close(), NULL)
  expect_error(bandit$get_context(), "Bandit subclass needs to implement")
  expect_error(bandit$get_reward(), "Bandit subclass needs to implement")
  expect_error(bandit$generate_bandit_data(), "Bandit subclass needs to implement")

})
