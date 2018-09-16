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

test_that("ContextualBernoulliBandit simulation", {

  bandit      <- ContextualBernoulliBandit$new(weights = matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))
  expect_identical(typeof(bandit), "environment")

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)
  expect_true(bandit$precaching)


  bandit      <- ContextualBernoulliBandit$new(weights = c(0.1, 0.9))
  expect_identical(typeof(bandit), "environment")

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

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret,3.4,tolerance = 0.00001)

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

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret,4.1,tolerance = 0.00001)

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


  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret,5.64,  tolerance = 0.001)

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

  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret,7.4, tolerance = 0.001)

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

  expect_equal(history$cumulative$LinUCBDisjointOptimized$cum_regret,55.4, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret,35.8, tolerance = 0.001)

})


test_that("BasicGaussianBandit", {

  horizon            <- 10
  sims               <- 10
  policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
  bandit             <- BasicGaussianBandit$new(c(0,0,1), c(1,1,1))
  agent              <- Agent$new(policy,bandit)

  history            <- Simulator$new(agent, horizon, sims, do_parallel = FALSE)$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret, 8.21, tolerance = 0.01)

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

  expect_equal(history$cumulative$Random$cum_regret,5.4, tolerance = 0.01)
  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret,1.1, tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret, 5.5, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret, 3.6, tolerance = 0.01)
  expect_equal(history$cumulative$ThompsonSampling$cum_regret,2.8, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$cum_regret,3.3, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonFirst$cum_regret,2, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret,1.7, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret,1.7, tolerance = 0.01)

})


test_that("BasicBernoulliBandit Long", {

  weight_per_arm     <- c(0.6, 0.1, 0.1)
  horizon            <- 1000
  simulations        <- 1

  bandit             <- BasicBernoulliBandit$new(weight_per_arm)

  prior              <- matrix(c(1,1,1,1,1,1),3,2) # arms x a/b

  agents             <- list(Agent$new(GittinsBrezziLaiPolicy$new(prior=prior), bandit))

  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret, 82, tolerance = 0.01)

})

test_that("ContextualBernoulliBandit MAB policies", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  bandit             <- ContextualBernoulliBandit$new(weights = weight_per_arm)

  agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                             Agent$new(OraclePolicy$new(), bandit),
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

  expect_equal(history$cumulative$Random$cum_regret,3.1, tolerance = 0.01)
  expect_equal(history$cumulative$Oracle$cum_regret,0, tolerance = 0.01)
  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret,1.5, tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret,2.9, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret,2.8, tolerance = 0.01)
  expect_equal(history$cumulative$ThompsonSampling$cum_regret,2.8, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$cum_regret,2.3, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonFirst$cum_regret, 1.3, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret,1.9, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret,2.1, tolerance = 0.01)

})

test_that("ContextualBernoulliBandit options", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  expect_error(ContextualBernoulliBandit$new(weights = weight_per_arm, reward_family = "notgood"))

  # without precaching

  bandit             <- ContextualBernoulliBandit$new(weights = weight_per_arm, precaching = FALSE)

  agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit))
  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret,  2.1, tolerance = 0.01)

  expect_message(bandit$generate_bandit_data(n = 1L, silent = FALSE), "Precaching bandit")

  # sum_weights = TRUE

  bandit             <- ContextualBernoulliBandit$new(weights = weight_per_arm, sum_weights = TRUE)

  agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit))
  simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history            <- simulation$run()

  expect_equal(history$cumulative$EpsilonGreedy$cum_regret,  2.3, tolerance = 0.01)

  expect_message(bandit$generate_bandit_data(n = 1L, silent = FALSE), "Precaching bandit")

})

test_that("ContinuumBandit", {

  horizon            <- 10
  simulations        <- 10

  continuous_arms  <- function(x) {
    -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4)
  }

  int_time    <- 100
  amplitude   <- 0.2
  learn_rate  <- 0.3
  omega       <- 2*pi/int_time
  x0_start    <- 2.0

  policy             <- LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start)

  bandit             <- ContinuumBandit$new(FUN = continuous_arms)

  agent              <- Agent$new(policy,bandit)

  history            <- Simulator$new(     agents = agent,
                                           horizon = horizon,
                                           simulations = simulations,
                                           do_parallel =  FALSE)$run()

  expect_equal(history$cumulative$Lif$reward,  2.8, tolerance = 0.01)

})

test_that("BasicBernoulliBandit MAB policies", {


  context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                    0.1, 0.8, 0.1,
                                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 40L
  simulations <- 1L
  bandit      <- ContextualBernoulliBandit$new(weights = context_weights, sum_weights = TRUE)

  # This can only be random policy, otherwise rejection sampling will
  # produce severely biased results.

  policy      <- RandomPolicy$new()

  agents <-
    list(
      Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
      Agent$new(LinUCBDisjointPolicy$new(0.6), bandit),
      Agent$new(OraclePolicy$new(), bandit)
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
  expect_equal(direct$get_cumulative_result(t=20)$Oracle$cum_reward,19)

  ########################### create random log data ################################

  context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                    0.1, 0.9, 0.1,
                                    0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 100L
  simulations <- 1L
  bandit      <- ContextualBernoulliBandit$new(weights = context_weights, sum_weights = TRUE)

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

  expect_equal(before$get_cumulative_result(t=20)$Random$cum_reward,16)
  expect_equal(before$get_cumulative_result(t=40)$Random$cum_reward,23)

  ######################## use the log to test a policy ##########################

  history <- History$new()
  history$load("test.RData")
  log_S <- history$get_data_table()

  bandit <- OfflinePolicyEvaluatorBandit$new(data_stream = log_S, k = 3, d = 3)

  agents <-
    list(
      Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
      Agent$new(LinUCBDisjointPolicy$new(0.6), bandit),
      Agent$new(OraclePolicy$new(), bandit)
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

  expect_equal(after$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,15)
  expect_equal(after$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,11)
  expect_equal(after$get_cumulative_result(t=20)$Oracle$cum_reward,17)

  expect_equal(after$get_cumulative_result(t=25)$LinUCBDisjoint$cum_reward,18)
  expect_equal(after$get_cumulative_result(t=25)$EpsilonGreedy$cum_reward,13)
  expect_equal(after$get_cumulative_result(t=25)$Oracle$cum_reward,22)

})
