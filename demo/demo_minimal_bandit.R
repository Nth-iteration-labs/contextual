#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

ptm <- proc.time()

CustomBanditLocal <- R6::R6Class(
  "CustomBanditLocal",
  inherit = BasicBandit,
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
simulation  <- Simulator$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

Plot$new()$grid(history)

print(sum(history$optimal)/sum(history$reward))

simulation$object_size()
