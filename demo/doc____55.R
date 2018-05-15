setwd("~/GitHub/contextual/demo")
source("dev.R")

 horizon            <- 100L
 simulations        <- 100L
 weights            <- c(0.9, 0.1, 0.1)

 policy             <- RandomPolicy$new(name = "Random")
 bandit             <- SyntheticBandit$new(weights = weights, precaching = FALSE)
 agent              <- Agent$new(policy, bandit)

 history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()

 plot(history, type = "arms")


###################

PoissonRewardBandit <- R6::R6Class(
  "PoissonRewardBandit",
  # Class extends BasicBandit
  inherit = BasicBandit,
  public = list(
    initialize = function(weights) {
      super$initialize(weights)
    },
    # Overrides BasicBandit's get_reward to generate Poisson based rewards
    get_reward = function(t, context, action) {
      reward_means = c(2,2,2)
      private$R <- matrix(rpois(3, reward_means) < self$get_weights(), self$k, self$d)*1
      list(
        reward                   = private$R[action$choice],
        optimal_reward_value     = private$R[which.max(private$R)]
      )
    }
  )
)
EpsilonGreedyAnnealingPolicy <- R6::R6Class(
  "EpsilonGreedyAnnealingPolicy",
  # Class extends EpsilonGreedyPolicy
  inherit = EpsilonGreedyPolicy,
  portable = FALSE,
  public = list(
    # Override get_action, use annealing epsilon
    get_action = function(t, context) {
      self$epsilon <- 1 / log(t + 0.0000001)
      super$get_action(t, context)
    }
  )
)
weights <- c(7,1,2)
bandit <- PoissonRewardBandit$new(weights)
agents <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "EG Annealing"), bandit),
                Agent$new(EpsilonGreedyAnnealingPolicy$new(0.1, "EG"), bandit) )
simulation <- Simulator$new(agents, horizon = 200L, simulations = 100L, do_parallel = FALSE)

history <- simulation$run()

par(mfrow = c(1, 2),mar = c(5, 5, 1, 1))
plot(history, type = "cumulative", grid = TRUE)
plot(history, type = "average", regret = FALSE, grid = TRUE)
