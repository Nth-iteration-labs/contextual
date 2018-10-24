context("Bandits")

test_that("Bandit superclass", {

  bandit      <- Bandit$new()
  expect_identical(typeof(bandit), "environment")

  expect_identical(bandit$post_initialization()$class_name, "Bandit")
  expect_identical(bandit$final(), NULL)
  expect_error(bandit$get_context(), "Bandit subclass needs to implement")
  expect_error(bandit$get_reward(), "Bandit subclass needs to implement")

})

test_that("ContextualPrecachingBandit simulation", {

  bandit      <- ContextualPrecachingBandit$new(weights = matrix(c(0.1, 0.9, 0.1, 0.9), 2, 2))
  expect_identical(typeof(bandit), "environment")

  expect_equal(bandit$k, 2)
  expect_equal(bandit$d, 2)

  bandit      <- ContextualPrecachingBandit$new(weights = c(0.1, 0.9))
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

  LinUCBGeneralPolicy

  policy        <- ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5)
  expect_identical(typeof(policy), "environment")

  agent         <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret,3.6,tolerance = 0.00001)


  ###############



  policy        <- LinUCBGeneralPolicy$new(0.6)
  expect_identical(typeof(policy), "environment")

  agent         <- Agent$new(policy, bandit)
  expect_identical(typeof(agent), "environment")

  simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
  history       <- simulation$run()

  expect_equal(history$cumulative$LinUCBGeneral$cum_regret,3.8,tolerance = 0.00001)


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

  expect_equal(history$cumulative$ContextualThompsonSampling$cum_regret,4,tolerance = 0.00001)

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
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit),
                        Agent$new(LinUCBDisjointPolicy$new(0.6), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$LinUCBDisjoint$cum_regret,7.4, tolerance = 0.001)
  expect_equal(history$cumulative$LinUCBDisjoint$cum_regret,
               history$cumulative$LinUCBDisjointOptimized$cum_regret)

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
                             Agent$new(EpsilonFirstPolicy$new(0.4,10), bandit),
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

  expect_equal(history$cumulative$Random$cum_regret,5.2, tolerance = 0.01)
  expect_equal(history$cumulative$GittinsBrezziLai$cum_regret,1, tolerance = 0.01)
  expect_equal(history$cumulative$Exp3$cum_regret, 6.1, tolerance = 0.01)
  expect_equal(history$cumulative$UCB1$cum_regret, 3.7, tolerance = 0.01)
  expect_equal(history$cumulative$ThompsonSampling$cum_regret,2.2 , tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$cum_regret,3.2, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonFirst$cum_regret,2.8, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret,1.3, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret,1.8, tolerance = 0.01)

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

test_that("ContextualPrecachingBandit MAB policies", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  bandit             <- ContextualPrecachingBandit$new(weights = weight_per_arm)

  agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                             Agent$new(OraclePolicy$new(), bandit),
                             Agent$new(EpsilonFirstPolicy$new(0.4,horizon), bandit),
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
  expect_equal(history$cumulative$EpsilonFirst$cum_regret, 3.5, tolerance = 0.01)
  expect_equal(history$cumulative$Softmax$cum_regret,1.9, tolerance = 0.01)
  expect_equal(history$cumulative$SimpleBTS$cum_regret,2.1, tolerance = 0.01)

})

test_that("ContextualPrecachingBandit options", {

  weight_per_arm     <- c(0.9, 0.1, 0.1)
  horizon            <- 10
  simulations        <- 10

  expect_error(ContextualPrecachingBandit$new(weights = weight_per_arm, reward_family = "notgood"))

})

test_that("ContinuumBandit", {

  horizon            <- 10
  simulations        <- 10

  continuous_arms  <- function(x) {
    -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4)
  }

  int_time    <- 5
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

  expect_equal(history$cumulative$Lif$reward,  2.84, tolerance = 0.01)

})

test_that("ContextualHybridBandit", {

  horizon       <- 10L
  simulations   <- 10L

  bandit        <- ContextualHybridBandit$new(k = 100, shared_features = 10, unique_features = 2)

  agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                        Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                        Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
                        Agent$new(ContextualEpochGreedyPolicy$new(8), bandit),
                        Agent$new(LinUCBHybridPolicy$new(0.6), bandit),
                        Agent$new(LinUCBHybridPolicy$new(0.6), bandit),
                        Agent$new(LinUCBHybridOptimizedPolicy$new(0.6), bandit),
                        Agent$new(GlmUCBPolicy$new(), bandit),
                        Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$ContextualThompsonSampling$reward,  0.9, tolerance = 0.01)
  expect_equal(history$cumulative$EpsilonGreedy$reward,  0.7, tolerance = 0.01)
  expect_equal(history$cumulative$LinUCBGeneral$reward,  0.8, tolerance = 0.01)
  expect_equal(history$cumulative$ContextualEpochGreedy$reward,  0.6, tolerance = 0.01)
  expect_equal(history$cumulative$LinUCBDisjointOptimized$reward,  0.7, tolerance = 0.01)
  expect_equal(history$cumulative$GlmUCB$cum_reward,  7.2, tolerance = 0.01)


  expect_equal(history$cumulative$LinUCBHybridOptimized$cum_reward, 7.7, tolerance = 0.01)
  expect_equal(history$cumulative$LinUCBHybrid$cum_reward,  history$cumulative$LinUCBHybridOptimized$cum_reward)


  horizon       <- 100L
  simulations   <- 1L

  bandit        <- ContextualHybridBandit$new(k = 10, shared_features = 10, unique_features = 2)

  agents        <- list(Agent$new(ContextualEpochGreedyPolicy$new(8), bandit))

  simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history        <- simulation$run()

  expect_equal(history$cumulative$ContextualEpochGreedy$cum_reward,  73, tolerance = 0.01)
})

test_that("ContextualPrecachingBandit GlmUCB", {


  context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                    0.1, 0.8, 0.1,
                                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)

  horizon     <- 300L
  simulations <- 1L
  bandit      <- ContextualPrecachingBandit$new(weights = context_weights)

  policy      <- RandomPolicy$new()

  agents <-
    list(
      Agent$new(GlmUCBPolicy$new(), bandit)
    )

  simulation  <-
    Simulator$new(
      agents,
      horizon = horizon,
      simulations = simulations,
      do_parallel = FALSE
    )

  history        <- simulation$run()

  # have to delve into this: why glmucb not always stable, 167, 169...

  expect_equal(history$cumulative$GlmUCB$cum_reward,  99, tolerance = 0.2)

})

test_that("ContextualBernoulliBandit", {


  horizon <- 20L
  simulations <- 10L

  weights <- matrix(c(0.8, 0.1, 0.1,
                      0.1, 0.8, 0.1,
                      0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)

  bandit <- ContextualBernoulliBandit$new(weights = weights)
  agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
                 Agent$new(ContextualEpsilonGreedy$new(0.1), bandit, "cEGreedy"),
                 Agent$new(ContextualLogitBTSPolicy$new(10), bandit, "LogitBTS"),
                 Agent$new(LinUCBDisjointPolicy$new(0.6), bandit, "LinUCB"))
  simulation <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history <- simulation$run()

  expect_equal(history$cumulative$EGreedy$cum_reward,  5.9, tolerance = 0.2)
  expect_equal(history$cumulative$cEGreedy$cum_reward,  9.9, tolerance = 0.2)
  expect_equal(history$cumulative$LogitBTS$cum_reward,  11.2, tolerance = 0.2)
  expect_equal(history$cumulative$LinUCB$cum_reward,  10.5, tolerance = 0.2)

})

test_that("ContextualBernoulliBandit", {


  horizon <- 20L
  simulations <- 10L

  weights <- matrix(c(0.8, 0.1, 0.1,
                      0.1, 0.8, 0.1,
                      0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)

  bandit <- ContextualBinaryBandit$new(weights = weights)
  agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
                 Agent$new(ContextualEpsilonGreedy$new(0.1), bandit, "cEGreedy"),
                 Agent$new(ContextualLogitBTSPolicy$new(10), bandit, "LogitBTS"),
                 Agent$new(LinUCBDisjointPolicy$new(0.6), bandit, "LinUCB"))
  simulation <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
  history <- simulation$run()

  expect_equal(history$cumulative$EGreedy$cum_reward,  6.4, tolerance = 0.2)
  expect_equal(history$cumulative$cEGreedy$cum_reward,  8.5, tolerance = 0.2)
  expect_equal(history$cumulative$LogitBTS$cum_reward,  7.3, tolerance = 0.2)
  expect_equal(history$cumulative$LinUCB$cum_reward,  8.5, tolerance = 0.2)

})

test_that("BasicBernoulliBandit MAB policies", {


  context_weights    <- matrix(  c( 0.8, 0.1, 0.1,
                                    0.1, 0.8, 0.1,
                                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 40L
  simulations <- 1L
  bandit      <- ContextualPrecachingBandit$new(weights = context_weights)

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

  expect_equal(direct$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,7)
  expect_equal(direct$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,6)
  expect_equal(direct$get_cumulative_result(t=20)$Oracle$cum_reward,13)

  ########################### create random log data ################################

  context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                    0.1, 0.9, 0.1,
                                    0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)
  horizon     <- 100L
  simulations <- 1L
  bandit      <- ContextualPrecachingBandit$new(weights = context_weights)

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

  expect_equal(before$get_cumulative_result(t=20)$Random$cum_reward,10)
  expect_equal(before$get_cumulative_result(t=40)$Random$cum_reward,15)

  ######################## use the log to test a policy ##########################

  history <- History$new()
  history$load("test.RData")
  log_S <- history$get_data_table()

  bandit <- OfflineReplayEvaluatorBandit$new(offline_data = log_S, k = 3, d = 3)

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

  expect_equal(after$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,8)
  expect_equal(after$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,5)
  expect_equal(after$get_cumulative_result(t=20)$Oracle$cum_reward,14)

  expect_equal(after$get_cumulative_result(t=25)$LinUCBDisjoint$cum_reward,10)
  expect_equal(after$get_cumulative_result(t=25)$EpsilonGreedy$cum_reward,6)
  expect_equal(after$get_cumulative_result(t=25)$Oracle$cum_reward,17)

  ######################## remove optimal values from log and try again ##########################

  history <- History$new()
  history$load("test.RData")
  log_S <- history$get_data_table()

  log_S$optimal_arm <- NULL
  log_S$optimal_reward <- NULL

  bandit <- OfflineReplayEvaluatorBandit$new(offline_data = log_S, k = 3, d = 3)

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

  expect_equal(after$get_cumulative_result(t=20)$LinUCBDisjoint$cum_reward,8)
  expect_equal(after$get_cumulative_result(t=20)$EpsilonGreedy$cum_reward,5)
  expect_equal(is.na(after$get_cumulative_result(t=20)$Oracle$cum_reward), TRUE)

  expect_equal(after$get_cumulative_result(t=25)$LinUCBDisjoint$cum_reward,10)
  expect_equal(after$get_cumulative_result(t=25)$EpsilonGreedy$cum_reward,6)
  expect_equal(is.na(after$get_cumulative_result(t=25)$Oracle$cum_reward), TRUE)

})


test_that("PropensityWeightingBandit", {

  horizon <- 1000
  simulations <- 1

  weights <- matrix( c(0.4, 0.3,
                       0.8, 0.7),

                     nrow = 2, ncol = 2, byrow = TRUE)

  BiasedPolicy <- R6::R6Class(
    portable = FALSE,
    class = FALSE,
    inherit = RandomPolicy,
    public = list(
      class_name = "BiasedPolicy",
      get_action = function(t, context) {
        if(context$X[1]==1) {           # 1: Male || 0: Female.

          prob <- c(0.75,0.25)            # Editor thinks men like Sport articles more.
        } else {
          prob <- c(0.25,0.75)            # Editor thinks women like Movie articles more.
        }
        action$choice               <- sample.int(context$k, 1, replace = TRUE, prob = prob)
        action$propensity           <- prob[action$choice]
        action
      }
    )
  )

  policy             <- BiasedPolicy$new()
  bandit             <- ContextualBernoulliBandit$new(weights = weights)
  agent              <- Agent$new(policy, bandit, "Random")

  simulation         <- Simulator$new(agent, horizon, simulations, save_context = TRUE, do_parallel = FALSE)
  history            <- simulation$run()

  b_dt               <- history$get_data_table()

  # ---

  bandit             <- OfflineReplayEvaluatorBandit$new(b_dt,2,2)
  policy             <- UCB1Policy$new()
  agent              <- Agent$new(policy, bandit, "rb")

  simulation         <- Simulator$new(agent, horizon, simulations, reindex = TRUE, do_parallel = FALSE)
  history            <- simulation$run()
  rb_dt              <- history$get_data_table()

  a <- sum(rb_dt[choice==1]$reward)/nrow(rb_dt[choice==1])
  b <- sum(rb_dt[choice==2]$reward)/nrow(rb_dt[choice==2])

  expect_equal(a,  0.416, tolerance = 0.02)
  expect_equal(b,  0.594, tolerance = 0.02)

  # ---

  bandit                 <- OfflinePropensityWeightingBandit$new(b_dt,2,2)
  policy                 <- UCB1Policy$new()
  agent                  <- Agent$new(policy, bandit, "prop")

  simulation             <- Simulator$new(agent, horizon, simulations, reindex = TRUE, do_parallel = FALSE)
  history                <- simulation$run()
  prop_dt                <- history$get_data_table()

  c <- sum(prop_dt[choice==1]$reward)/nrow(prop_dt[choice==1])
  d <- sum(prop_dt[choice==2]$reward)/nrow(prop_dt[choice==2])

  expect_equal(c,  0.636, tolerance = 0.02)
  expect_equal(d,  0.483, tolerance = 0.02)

})

