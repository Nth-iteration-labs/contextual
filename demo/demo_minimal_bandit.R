########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

set.seed(21)

ptm <- proc.time()

CustomBanditLocal <- R6::R6Class(
  "CustomBanditLocal",
  inherit = AbstractBandit,
  portable = FALSE, class = FALSE,
  public = list(
    initialize   = function() {
      super$initialize()
      self$set_weights(c(0.1,0.1,0.9))
    }
  )
)

bandit      <- CustomBanditLocal$new()
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorParallel$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

Plot$new()$set_external(T, 11, 6L)$grid(history)

print(sum(history$optimal)/sum(history$reward))

