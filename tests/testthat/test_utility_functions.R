context("Utility Functions")

test_that("Inc and Dec", {

  x <- 1:5
  inc(x) <- 5
  expect_equal(6:10, x)

  dec(x) <- 5
  expect_equal(1:5, x)

})

test_that("Welford", {
  set.seed(42)
  v <- sample(20)
  s <- var_welford(v)
  expect_equal(s,35)
})

test_that("Formatted difftime", {
  ft <- formatted_difftime(difftime(strftime ("2019-10-18 13:35:35 CEST"),
                                    strftime ("2018-09-17 12:31:30 CEST")))
  expect_equal(ft,"396 days, 1:04:05")
})

test_that("Inverse Logit", {
  expect_equal(inv_logit(10),0.9999546, tolerance = 0.002)
})

test_that("InvGamma", {

  s <- seq(0, 5, .01)
  x <- dinvgamma(s, 7, 10)
  x2 <- dinvgamma(s, 7, scale = 0.10)

  expect_equal_to_reference(x, file = "dinvgamma.rds")
  expect_equal_to_reference(x2, file = "dinvgamma.rds")

  x <- dinvgamma(s, 7, 10, log = TRUE)
  expect_equal_to_reference(x, file = "dinvgamma_log.rds")

  q <- 2
  (p <- pinvgamma(q, 7, 10))
  expect_equal(qinvgamma(p, 7, 10), q)

  q <- 2
  (p <- pinvgamma(q, 7, scale = 0.10))
  expect_equal(qinvgamma(p, 7, scale = 0.10), q)

  expect_equal(mean(rinvgamma(1e5, 7, 10) <= q),0.76088, tolerance = 0.002)

  expect_equal(mean(rinvgamma(1e5, 7, scale = 0.10) <= q),0.763, tolerance = 0.02)

})

