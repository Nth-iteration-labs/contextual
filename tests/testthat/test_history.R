context("History")

test_that("History simulation, saving and loading", {

  bandit      <- SyntheticBandit$new()
  bandit$set_weights(matrix(c(0.9, 0.1, 0.1,
                              0.1, 0.2, 0.1,
                              0.2, 0.1, 0.2),
                            nrow = 3L,
                            ncol = 3L))
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <-
    Simulator$new(
      agent,
      horizon = 30L,
      simulations = 30L,
      worker_max = 1
    )
  history     <- simulation$run()
  expect_equal(history$data$reward[1], 0)

  expect_warning(history$data <- 1, ".*read only.*")

  reward = list()
  reward["reward"]  = 10
  reward["choice"] = 1
  reward["is_optimal"] = 0
  reward["oracle"] = 1
  reward["propensity"] = 0

  action = list()
  action$choice = 2
  action$optimal_choice = 2

  history$save (
    index = 1,
    t = 30,
    action = action,
    reward = reward,
    policy_name = "EpsilonGreedy",
    s = 30
  )
  expect_equal(history$data$reward[1], 10)

  history$save_data("test.RData")
  expect_true(file.exists("test.RData"))
  history$reset()
  history$load_data("test.RData")
  file.remove("test.RData")
  expect_equal(history$data$reward[1], 10)

  df <- history$get_data_frame()
  history$reset()
  history$set_data_frame(df)
  expect_equal(history$data$reward[1], 10)

  dt <- history$get_data_table()
  history$reset()
  history$set_data_table(dt)
  expect_equal(history$data$reward[1], 10)
  expect_equal(nrow(history$data), 900)

  history$save(
    index = 30,
    t = 0,
    action = action,
    reward = reward,
    policy_name = "EpsilonGreedy",
    s = 0
  )
  history$delete_empty_rows()
  expect_equal(nrow(history$data), 899)
})

test_that("History simulation testing context and theta.", {

  bandit      <- SyntheticBandit$new()
  bandit$set_weights(matrix(c(0.9, 0.1, 0.1,
                              0.1, 0.2, 0.1,
                              0.2, 0.1, 0.2),
                            nrow = 3L,
                            ncol = 3L))
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <-
    Simulator$new(
      agent,
      horizon = 30L,
      simulations = 30L,
      worker_max = 1,
      save_context = TRUE,
      save_theta = TRUE
    )
  history     <- simulation$run()
  expect_equal(unlist(history$data$context[1]), c(1,1,0))
  expect_equal(history$data$theta[[1]]$n[[2]], 1)

})

test_that("History simulation testing save_context.", {

  bandit      <- SyntheticBandit$new()
  bandit$set_weights(matrix(c(0.9, 0.1, 0.1,
                              0.1, 0.2, 0.1,
                              0.2, 0.1, 0.2),
                            nrow = 3L,
                            ncol = 3L))
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <-
    Simulator$new(
      agent,
      horizon = 3L,
      simulations = 3L,
      worker_max = 1,
      save_context = TRUE,
      save_theta = FALSE
    )
  history     <- simulation$run()
  expect_equal(unlist(history$data$context[1]), c(1,1,0))
  expect_equal(history$data$theta[[1]][[2]]$n, NULL)

})

test_that("History simulation testing save_theta", {

  bandit      <- SyntheticBandit$new()
  bandit$set_weights(matrix(c(0.9, 0.1, 0.1,
                              0.1, 0.2, 0.1,
                              0.2, 0.1, 0.2),
                            nrow = 3L,
                            ncol = 3L))
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <-
    Simulator$new(
      agent,
      horizon = 3L,
      simulations = 3L,
      worker_max = 1,
      save_context = FALSE,
      save_theta = TRUE
    )
  history     <- simulation$run()
  expect_equal(unlist(history$data$context[1]), NULL)
  expect_equal(history$data$theta[[1]]$n[[2]], 1)

})
