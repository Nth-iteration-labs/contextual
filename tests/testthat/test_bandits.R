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


test_that("BasicGaussianBandit", {

  horizon            <- 10
  sims               <- 10
  policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
  bandit             <- BasicGaussianBandit$new(c(0,0,1), c(1,1,1))
  agent              <- Agent$new(policy,bandit)

  history            <- Simulator$new(agent, horizon, sims, do_parallel = FALSE)$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret_var, 15.8, tolerance = 0.01)

})

test_that("BasicBernoulliBandit MAB policies", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  bandit             <- BasicBernoulliBandit$new(weight_per_arm)

  agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                             Agent$new(EpsilonFirstPolicy$new(4), bandit),
                             Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                             Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                             Agent$new(Exp3Policy$new(0.1), bandit),
                             Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                             Agent$new(UCB1Policy$new(), bandit),
                             Agent$new(SoftmaxPolicy$new(0.1), bandit),
                             Agent$new(SimpleBTSPolicy$new(), bandit)
  )

  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$Random$cum_regret_var,1.82, tolerance = 0.01)
  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret_var,1.43, tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret_var,1.17, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret_var,0.711, tolerance = 0.01)
  expect_equal(history$cumulative$ThompsonSampling$cum_regret_var,1.07, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$cum_regret_var,13.6, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonFirst$cum_regret_var,1.11, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret_var,3.79, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret_var,3.12, tolerance = 0.01)

})

test_that("ContextualWeightBandit MAB policies", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  bandit             <- ContextualWeightBandit$new(weights = weight_per_arm)

  agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                             Agent$new(EpsilonFirstPolicy$new(4), bandit),
                             Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                             Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                             Agent$new(Exp3Policy$new(0.1), bandit),
                             Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                             Agent$new(UCB1Policy$new(), bandit),
                             Agent$new(SoftmaxPolicy$new(0.1), bandit),
                             Agent$new(SimpleBTSPolicy$new(), bandit)

  )

  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$Random$cum_regret_var,2.84, tolerance = 0.01)
  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret_var,2.89, tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret_var,2.9, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret_var,2.72, tolerance = 0.01)
  expect_equal(history$cumulative$ThompsonSampling$cum_regret_var,1.66, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$cum_regret_var,4.93, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonFirst$cum_regret_var,0.989, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret_var,2.77, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret_var,1.43, tolerance = 0.01)

})

test_that("ContextualWeightBandit options", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  expect_error(ContextualWeightBandit$new(weights = weight_per_arm, reward_family = "notgood"))

  # Gaussian

  bandit             <- ContextualWeightBandit$new(weights = weight_per_arm, reward_family = "Gaussian")
  agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit))
  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret_var, 6.52, tolerance = 0.01)

  # Poisson, without precaching

  bandit             <- ContextualWeightBandit$new(weights = weight_per_arm,
                                                   reward_family = "Poisson",
                                                   precaching = FALSE)

  agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit))
  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret_var,  14.1, tolerance = 0.01)

  expect_message(bandit$generate_bandit_data(n = 1L, silent = FALSE), "Precaching bandit")

})

test_that("BasicBernoulliBandit MAB policies", {


  context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                    0.1, 0.8, 0.1,
                                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 40L
  simulations <- 1L
  bandit      <- ContextualWeightBandit$new(weights = context_weights, sum_weights = TRUE)

  # This can only be random policy, otherwise rejection sampling will
  # produce severely biased results.

  policy      <- RandomPolicy$new()

  agents <-
    list(
      Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
      Agent$new(LinUCBDisjointPolicy$new(0.6), bandit)
    )

  simulation  <-
    Simulator$new(
      agents,
      horizon = horizon,
      simulations = simulations,
      save_context = TRUE,
      do_parallel = FALSE
    )

  direct <- simulation$run()

  expect_equal(direct$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,15)
  expect_equal(direct$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,8)

  ########################### create random log data ################################

  context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                    0.1, 0.9, 0.1,
                                    0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 100L
  simulations <- 1L
  bandit      <- ContextualWeightBandit$new(weights = context_weights, sum_weights = TRUE)

  # This can only be random policy, otherwise rejection sampling will
  # produce severely biased results.

  policy      <- RandomPolicy$new()

  agent       <- Agent$new(policy, bandit)

  simulation  <-
    Simulator$new(
      agent,
      horizon = horizon,
      simulations = simulations,
      save_context = TRUE,
      do_parallel = FALSE
    )

  before <- simulation$run()
  before$save("test.RData")

  expect_equal(before$get_cumulative_result(t=20)$Random$cum_reward,17)
  expect_equal(before$get_cumulative_result(t=40)$Random$cum_reward,27)

  ######################## use the log to test a policy ##########################

  history <- History$new()
  history$load("test.RData")
  log_S <- history$get_data_table()

  bandit <- LiSamplingOfflineBandit$new(data_stream = log_S, k = 3, d = 3)

  agents <-
    list(
      Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
      Agent$new(LinUCBDisjointPolicy$new(0.6), bandit)
    )

  simulation <-
    Simulator$new(
      agents,
      horizon = horizon,
      simulations = simulations,
      t_over_sims = TRUE,
      do_parallel = FALSE,
      reindex = TRUE
    )

  after <- simulation$run()
  if (file.exists("test.RData")) file.remove("test.RData")

  expect_equal(after$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,16)
  expect_equal(after$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,13)

  expect_equal(after$get_cumulative_result(t=30)$LinUCBDisjoint$cum_reward,26)
  expect_equal(after$get_cumulative_result(t=30)$EpsilonGreedy$cum_reward,17)

})
