library(R6)
# library(contextual)

setwd("~/GitHub/contextual/scripts")

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/utility.R")
source("../R/simulation_basic.R")
source("../R/bandit_gaussian.R")
source("../R/agent_gradient.R")
source("../R/policy_epsilon_greedy.R")

EGreedyComparison <- R6Class(

  "EGreedyComparison",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    label = "",
    bandit = NULL,
    agents = NULL,
    initialize = function() {
      self$label = 'Action-Value Methods'

      self$bandit = GaussianBandit$new(10)                          # generator, really ... BanditGenerator.. Other option is BanditData

      self$agents = list(
        GradientAgent$new(EpsilonGreedyPolicy$new(0.1), bandit ),   # ok, this seems fine .. and agent uses a policy to solve a bandit
        GradientAgent$new(EpsilonGreedyPolicy$new(0.4), bandit ),   # the bandit generates its data, or is given a stash of data,
        GradientAgent$new(EpsilonGreedyPolicy$new(0.6), bandit )    # or we give access to a real life bandit data stream :)
      )
    }
  )
)

iterations      = 10
horizon         = 1000

comparison      = EGreedyComparison$new()

simulation      = BasicSimulation$new(comparison)

results         = simulation$run(horizon, iterations)

matplot( results$reward,         type = "l", lty = 1, xlab = "Time Step", ylab = "Average Reward")
matplot( results$optimal * 100,  type = "l", lty = 1, xlab = "Time Step", ylab = "Optimal Action", ylim = c(0,100))
