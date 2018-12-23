library(contextual)

# Replication of THOMPSON SAMPLING WITH THE ONLINE BOOTSTRAP By Dean Eckles and Maurits Kaptein

# https://arxiv.org/abs/1410.4009

# Fig 2. Empirical regret for Thompson sampling and BTS in a K-armed binomial bandit problem with
# varied differences between the optimal arm and all others.

bandit             <- BasicBernoulliBandit$new(weights = c(0.5, rep(0.4,9)))

agents             <- list(Agent$new(BootstrapTSPolicy$new(1000), bandit, "BTS 1000"),
                           Agent$new(ThompsonSamplingPolicy$new(), bandit, "TS"))

simulator          <- Simulator$new(agents        = agents,
                                    do_parallel   = TRUE,
                                    save_interval = 50,
                                    set_seed      = 999,
                                    horizon       = 1e+05,
                                    simulations   = 1000)

simulator$run()

plot(simulator$history, log = "x")
