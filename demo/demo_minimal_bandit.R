#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

ptm <- proc.time()

CustomBanditLocal <- R6::R6Class(
  "CustomBanditLocal",
  inherit = BasicBandit,
  portable = TRUE, class = TRUE,
  public = list(
    initialize   = function(weights) {
      super$initialize(weights)
    }
  )
)
# only works because init with 3  ###########################
bandit      <- CustomBanditLocal$new(c(0.9,0.1,0.1))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <- Simulator$new(agent, horizon = 100L, simulations = 100L, do_parallel = FALSE)

history     <- simulation$run()

plot(history, type = "grid")

print(sum(history$optimal)/sum(history$reward))

simulation$object_size()
