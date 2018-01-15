context("Plot")

test_that("Plot check", {

  bandit      <- BasicBandit$new()
  bandit$set_weights(matrix(c(0.1,0.9,0.1,0.9),2,2))
  bandit$set_weights(c(0.1,0.9))
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()

  plot = Plot$new()
  plot_result = plot$grid(history)

  expect_equal(plot_result$bandit_matrix,4)
  expect_true(file.exists("Rplots.pdf"))

})
