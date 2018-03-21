setwd("~/GitHub/contextual/demo")
source("dev.R")


horizon            <- 1000L
simulations        <- 3L
bandit             <- ContextualBandit$new(k = 5, d = 6, num_users = 0)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(0.7, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)

history            <- simulation$run()

plot(history, type = "cumulative", rate = TRUE)

###

horizon            <- 1000L
simulations        <- 3L
bandit             <- ContextualBandit$new(k = 5, d = 6, num_users = 7)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(0.7, "LinUCB"), bandit),
                            Agent$new(LinUCBHybridPolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)

history            <- simulation$run()

plot(history, type = "cumulative", rate = TRUE)
