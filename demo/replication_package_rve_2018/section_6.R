library(contextual)

BasicPoissonBandit <- R6::R6Class(
  inherit = BasicBernoulliBandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    weights = NULL,
    class_name = "BasicPoissonBandit",
    # OVerride get_reward & generate Poisson based rewards
    get_reward = function(t, context, action) {
      reward_means = rep(2,self$k)
      rpm <- rpois(self$k, reward_means)
      rewards <- matrix(rpm < self$weights, self$k, 1)*1
      optimal_arm    <- which.max(rewards)
      reward  <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)

EpsilonGreedyAnnealingPolicy <- R6::R6Class(
  # Class extends EpsilonGreedyPolicy
  inherit = EpsilonGreedyPolicy,
  portable = FALSE,
  public = list(
    class_name = "EpsilonGreedyAnnealingPolicy",
    # Override EpsilonGreedyPolicy's get_action, use annealing epsilon
    get_action = function(t, context) {
      self$epsilon <- 1 / log(t + 0.0000001)
      super$get_action(t, context)
    }
  )
)

weights <- c(7,1,2)
horizon <- 200
simulations <- 1000
bandit <- BasicPoissonBandit$new(weights)
eg_policy  <- EpsilonGreedyPolicy$new(0.1)
ega_policy <- EpsilonGreedyAnnealingPolicy$new(0.1)
agents <- list(Agent$new(ega_policy, bandit, "EG Annealing"),
               Agent$new(eg_policy, bandit, "EG"))
simulation <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history <- simulation$run()

plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE,
              legend_position = "bottomright")
plot(history, type = "arms",  limit_agents = c("EG Annealing"), no_par = TRUE,
              interval = 25)
plot(history, type = "arms",  limit_agents = c("EG"), no_par = TRUE,
              interval = 25)
