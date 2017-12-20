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
    },
    get_reward = function(action, t) {
      self$calculate_reward( runif(self$k) < self$get_weights() )
      self$format_reward(action)
    }
  )
)

bandit      <- CustomBanditLocal$new()
policy      <- EpsilonGreedyPolicy$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

print(proc.time() - ptm)

Plot$new()$set_external(T, 11, 6L)$grid(history)

#print(sum(history$optimal)/sum(history$reward))

