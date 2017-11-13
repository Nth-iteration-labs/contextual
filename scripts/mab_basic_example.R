library(R6)

# library(contextual)

setwd("~/GitHub/contextual/scripts")

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/utility.R")
source("../R/environment_basic.R")
source("../R/bandit_gaussian.R")
source("../R/agent_gradient.R")
source("../R/policy_epsilon_greedy.R")

EpsilonGreedyExample <- R6Class(

  "EpsilonGreedyExample",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    label = NULL,
    bandit = NULL,
    agents = NULL,
    initialize = function(epsilon = NA) {
      self$label = 'Action-Value Methods'
      self$bandit = GaussianBandit$new(10)
      self$agents = list(
        GradientAgent$new(bandit, EpsilonGreedyPolicy$new(0.1)),
        GradientAgent$new(bandit, EpsilonGreedyPolicy$new(0.4)),
        GradientAgent$new(bandit, EpsilonGreedyPolicy$new(0.6))
      )
    }

  )
)

experiments    = 300
trials         = 300

example        = EpsilonGreedyExample$new()
env            = BasicEnvironment$new(example$bandit, example$agents, example$label)
results        = env$run(trials, experiments)

matplot( results$reward,         type = "l", lty = 1, xlab = "Time Step", ylab = "Average Reward")
matplot( results$optimal * 100,  type = "l", lty = 1, xlab = "Time Step", ylab = "Optimal Action", ylim = c(0,100))



