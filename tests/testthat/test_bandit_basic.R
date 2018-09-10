context("Bandit")

test_that("Bandit simulation", {

  bandit      <- Bandit$new()
  expect_identical(typeof(bandit), "environment")

})
