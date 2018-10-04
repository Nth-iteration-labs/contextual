library(contextual)
library(here)

setwd(here::here("demo","alt_par_backend_examples","redis"))

source("simulator_redis.R")

library(contextual)

horizon       <- 1000L
simulations   <- 4L

bandit        <- ContextualLinearBandit$new(k = 5, d = 5)

agents        <-list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                     Agent$new(ContextualLogitBTSPolicy$new(10), bandit),
                     Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0), bandit))

simulation     <- RedisSimulator$new(agents, horizon, simulations)

history        <- simulation$run()

plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")
