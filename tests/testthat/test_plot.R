context("Plot")

test_that("Plot check", {
  arm_weights         <- c( 0.9, 0.1)
  bandit      <- SyntheticBandit$new(arm_weights = arm_weights)
  policy      <- EpsilonGreedyPolicy$new()
  agent       <- Agent$new(policy, bandit)
  simulation  <- Simulator$new(agent, horizon = 10L, simulations = 10L, worker_max = 1)
  history     <- simulation$run()

  plot = Plot$new()
  plot_result = plot$grid(history)

  #expect_equal(plot_result$bandit_matrix,4)
  expect_true(file.exists("Rplots.pdf"))

})
