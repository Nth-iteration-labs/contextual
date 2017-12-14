setwd("~/GitHub/contextual/demo")
#library(contextual)
source("dev.R")

set.seed(21L)

bandit <- SyntheticBandit$new(
  k = 3L,
  d = 3L,
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)

                     #d1  #d2  #d3
bandit$set_weights(c(0.9, 0.0, 0.1,  #k1                                        # override auto-generated weights
                     0.1, 0.9, 0.1,  #k2                                        # d: how many features
                     0.9, 0.1, 0.9)) #k3                                        # k: how many arms

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

ptm <- proc.time()                                                              # or Rprof ( tf <- "log.log",  memory.profiling = TRUE )

simulations    <- 100L
horizon        <- 100L
simulation     <- SimulatorParallel$new(agents)
history        <- simulation$run(horizon, simulations)

print(proc.time() - ptm)                                                        # or Rprof ( NULL ) ; print ( summaryRprof ( tf )  )

plot <- Plot$new()$set_external(T, 11, 6L)                                      # initialize plot.. TODO: change to within class
print(plot$grid(history))                                                       # plot the results...

