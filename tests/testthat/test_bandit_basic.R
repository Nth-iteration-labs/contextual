context("BasicBandit")

test_that("BasicBandit simulation", {

  bandit      <- BasicBandit$new()
  expect_identical(typeof(bandit), "environment")

})
