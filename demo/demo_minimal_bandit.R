########################### package dev helpers ################################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

################################################################################

set.seed(21)

ptm <- proc.time()

CustomBanditLocal <- R6::R6Class(
  "BasicMabBandit",
  inherit = AbstractBandit,
  portable = FALSE, class = FALSE,
  public = list(
    initialize   = function() {
      self$set_precaching(FALSE)
      self$set_weights(c(0.1,0.1,0.9))
    },
    get_reward = function(action, t) {
      self$calculate_reward( runif(self$k) < self$get_weights() )
      self$do_reward(action)
    }
  )
)

EpsilonGreedyPolicyLocal <- R6::R6Class(
  "EpsilonGreedyPolicy",
  portable = FALSE, class = FALSE,
  inherit = AbstractPolicy,
  public = list(
    epsilon = NULL,
    initialize = function(epsilon = 0.1, name = "EpsilonGreedy") {
      super$initialize(name)
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$parameters <- list('chosen' = 0, 'succes' = 0, 'value' = 0)
    },
    get_action = function(context) {
      if (runif(1) < self$epsilon) {
        self$action$choice  <- sample.int(context$k, 1, replace = TRUE)
      } else {
        self$action$choice <- self$argmaxlist(self$theta,"value")
      }
      self$action
    },
    set_reward = function(reward, context) {
      self$theta[[reward$choice]]$chosen <- self$theta[[reward$choice]]$chosen + 1
      if (reward$reward == 1) self$theta[[reward$choice]]$succes <- self$theta[[reward$choice]]$succes + 1
      self$theta[[reward$choice]]$value <- self$theta[[reward$choice]]$value +
        (1 / self$theta[[reward$choice]]$chosen) *
        (reward$reward - self$theta[[reward$choice]]$value)
      self$theta
    }
  )
)

bandit      <- CustomBanditLocal$new()
policy      <- EpsilonGreedyPolicyLocal$new(0.05)
agent       <- Agent$new(policy, bandit)
simulation  <- SimulatorBasic$new(agent, horizon = 100L, simulations = 100L)

history     <- simulation$run()

print(proc.time() - ptm)

Plot$new()$set_external(T, 11, 6L)$grid(history)
