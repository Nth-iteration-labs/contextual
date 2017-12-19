########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

set.seed(12)

################################################################################

ptm <- proc.time()                                                              # or Rprof ( tf <- "log.log",  memory.profiling = TRUE )

############################# basic simulation #################################

bandit      <- SyntheticBandit$new()$set_weights(c(0.1, 0.1, 0.9))
policy      <- EpsilonGreedyPolicy$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

################################################################################

print(proc.time() - ptm)                                                        # or Rprof ( NULL ) ; print ( summaryRprof ( tf )  )

################################################################################

Plot$new()$set_external(T, 11, 6L)$grid(history)
