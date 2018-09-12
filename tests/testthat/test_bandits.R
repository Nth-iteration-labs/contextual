context("Bandits")

test_that("Bandit superclass", {

  bandit      <- Bandit$new()
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$post_initialization()$class_name, "Bandit")
  expect_identical(bandit$close(), NULL)
  expect_error(bandit$get_context(), "Bandit subclass needs to implement")
  expect_error(bandit$get_reward(), "Bandit subclass needs to implement")
  expect_error(bandit$generate_bandit_data(), "Bandit subclass needs to implement")

})

test_that("ContextualWeightBandit simulation", {
  bandit      <- ContextualWeightBandit$new()
  expect_identical(typeof(bandit), "environment")

  bandit$set_weights(matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_true(bandit$precaching)

  bandit$set_weights(c(0.1, 0.9))
  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 1)

  policy      <- EpsilonGreedyPolicy$new()
  expect_identical(typeof(policy), "environment")

  agent       <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

})


test_that("ContextualLogitBandit intercept TRUE", {

  bandit      <- ContextualLogitBandit$new(k = 5, d = 5, intercept = TRUE)
  expect_identical(typeof(bandit), "environment")

  horizon       <- 10L
  simulations   <- 10L

  policy        <- ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5)
  expect_identical(typeof(policy), "environment")

  agent         <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret_var,2.7111111111,tolerance = 0.00001)

})

test_that("ContextualLogitBandit - intercept FALSE", {

  bandit      <- ContextualLogitBandit$new(k = 5, d = 5, intercept = FALSE)
  expect_identical(typeof(bandit), "environment")

  horizon       <- 10L
  simulations   <- 10L

  policy        <- ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5)
  expect_identical(typeof(policy), "environment")

  agent         <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret_var,3.6555555556,tolerance = 0.00001)

})

test_that("ContextualLinearBandit, binary_rewards  = FALSE", {

  horizon       <- 10L
  simulations   <- 10L

  bandit        <- ContextualLinearBandit$new(k = 5, d = 5)
  expect_identical(typeof(bandit), "environment")

  agents        <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()


  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret_var,4.00078,  tolerance = 0.00001)

})

test_that("ContextualLinearBandit, binary_rewards = TRUE", {

  horizon       <- 10L
  simulations   <- 10L

  bandit        <- ContextualLinearBandit$new(k = 5, d = 5, binary_rewards = TRUE)
  expect_identical(typeof(bandit), "environment")

  agents        <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret_var,1.822222, tolerance = 0.00001)

})


test_that("ContextualWheelBandit", {

  horizon       <- 10L
  simulations   <- 10L

  delta         <- 0.95
  num_actions   <- 5
  context_dim   <- 2
  mean_v        <- c(1.0, 1.0, 1.0, 1.0, 1.2)
  std_v         <- c(0.05, 0.05, 0.05, 0.05, 0.05)
  mu_large      <- 50
  std_large     <- 0.01

  bandit        <- ContextualWheelBandit$new(delta, mean_v, std_v, mu_large, std_large)
  expect_identical(typeof(bandit), "environment")

  agents        <- list(Agent$new(UCB1Policy$new(), bandit),
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret_var,2360.296, tolerance = 0.001)
  expect_equal(history$cumulative$UCB1$cum_regret_var,1087, tolerance = 0.001)


})




