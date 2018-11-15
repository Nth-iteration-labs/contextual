library(contextual)

# Replication of THOMPSON SAMPLING WITH THE ONLINE BOOTSTRAP By Dean Eckles and Maurits Kaptein

# https://arxiv.org/abs/1410.4009

# Fig 3: Comparison of empirical regret for BTS with varied number of bootstrap replicates.

# Sim completes within an hour on a 12 core / 24 threads server.

policy             <- ThompsonSamplingPolicy$new()

bandit             <- BasicBernoulliBandit$new(weights = c(0.5, rep(0.4,9)))

agents             <- list(Agent$new(SimpleBTSPolicy$new(10), bandit, "BTS 10"),
                           Agent$new(SimpleBTSPolicy$new(100), bandit, "BTS 100"),
                           Agent$new(SimpleBTSPolicy$new(1000), bandit, "BTS 1000"),
                           Agent$new(SimpleBTSPolicy$new(10000), bandit, "BTS 10000"))

simulator          <- Simulator$new(agents        = agents,
                                    do_parallel   = TRUE,
                                    save_interval = 50,
                                    horizon       = 1e+05,
                                    simulations   = 1000)

simulator$run()

plot(simulator$history, type = "cumulative", regret = TRUE, log = "x")
