#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

PoissonRewardBandit <- R6::R6Class(
  "PoissonRewardBandit",
  # Class extends BasicBandit
  inherit = BasicBandit,
  public = list(
    initialize   = function(weights) {
      super$initialize(weights)
    },
    # Overrides BasicBandit's do_action to generate Poisson based rewards
    do_action = function(context, action, t) {
      reward_means <- c(2,2,2)
      private$R    <- matrix(rpois(3, reward_means) < self$get_weights(), self$k, self$d)*1
      rewardlist   <- list(
        reward     = private$R[action$choice],
        optimal_reward_value     = private$R[which.max(private$R)]
      )
      rewardlist
    }
  )
)

EpsilonGreedyAnnealingPolicy <- R6::R6Class(
  "EpsilonGreedyAnnealingPolicy",
  # Class extends EpsilonGreedyPolicy
  inherit = EpsilonGreedyPolicy,
  portable = FALSE,
  class = FALSE,
  public = list(
    get_action = function(context, t) {
      # Override get_action to make annealing
      epsilon = 1 / log(t + 0.0000001)
      if (runif(1) > epsilon) {
        action$choice <- max_in(theta$mean)
        action$propensity <- 1 - self$epsilon
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
        action$propensity <- epsilon*(1/context$k)
      }
      action
    }
  )
)

weights     <- c(7,1,2)
bandit      <- PoissonRewardBandit$new(weights)
agents      <- list( Agent$new(EpsilonGreedyPolicy$new(0.2, "EG"), bandit),
                     Agent$new(EpsilonGreedyAnnealingPolicy$new(0.2, "EG Annealing"), bandit) )
simulation  <- Simulator$new(agents, horizon = 600L, simulations = 50L)

history     <- simulation$run()

par(mfrow = c(1, 2),mar = c(5, 5, 1, 1))
plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "average", regret = FALSE)
