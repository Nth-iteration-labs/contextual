context("LiLogBandit")

test_that("LiLogBandit simulation", {

  bandit      <- SyntheticBandit$new()
  bandit$set_weights(matrix(
    c(0.9, 0.0, 0.1,
      0.1, 0.9, 0.1,
      0.1, 0.1, 0.9),
    nrow = 3,
    ncol = 3
  ))

  policy      <- RandomPolicy$new()

  agent       <- Agent$new(policy, bandit)

  simulation  <-
    Simulator$new(
      agent,
      horizon = 10L,
      simulations = 10L,
      save_context = TRUE,
      save_theta = FALSE,
      worker_max = 1
    )

  before <- simulation$run()

  before$save_data("test.RData")

  compare_before_reward <- c(
    1,1,0,0,0,0,1,0,0,1,1,1,1,1,1,1,0,
    0,0,0,0,1,0,1,1,0,0,0,1,1,1,0,1,1,
    1,0,1,1,0,1,0,0,0,0,1,1,1,1,1,0,0,
    1,0,0,0,1,0,0,0,0,0,1,0,1,1,1,0,0,
    0,0,0,0,0,0,0,0,1,1,0,0,1,0,1,1,0,
    0,0,0,1,1,0,0,0,1,1,0,0,0,0,0
  )
  expect_equal(before$data$reward,compare_before_reward)

  ### use the log to test a policy ###

  log_S     <- History$new()
  log_S$load_data("test.RData")

  bandit      <- LiLogBandit$new(log_S, 3, 3)

  policy      <- LinUCBPolicy$new(1.0)
  agent       <- Agent$new(policy, bandit)
  simulation  <-
    Simulator$new(
      agent,
      horizon = 10L,
      simulations = 10L,
      worker_max = 1,
      continouous_counter = TRUE
    )

  after <- simulation$run()

  compare_after_reward <- c(
    1,0,0,0,0,1,1,1,0,0,1,0,1,1,1,1,0,1,0,0,0,1,
    1,0,0,1,0,1,0,0,1,0,0,0,0,1,1,1,0,0,1,0,0
  )
  expect_equal(after$data$reward,compare_after_reward)

  if (file.exists("test.RData")) file.remove("test.RData")
})
