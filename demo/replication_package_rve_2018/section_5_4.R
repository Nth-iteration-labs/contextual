library(contextual)

#' @export
BasicContextualBernoulliPrecachingBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    weights = NULL,
    class_name = "NaiveContextualBernoulliPrecachingBandit",
    initialize = function(weights) {
      self$weights     <- weights        # d x k weight matrix
      self$d           <- nrow(weights)  # d features
      self$k           <- ncol(weights)  # k arms
    },
    get_context = function(t) {
      # generate d dimensional feature vector, one random feature active
      Xa <- sample(c(1,rep(0,self$d-1)))
      # convert to d x k matrix: one feature vector, recycled to every arm
      X  <- matrix(Xa, self$d, self$k)
      context <- list(
        X = X,
        k = self$k,
        d = self$d
      )
    },
    get_reward = function(t, context, action) {
      # which arm was selected?
      arm            <- action$choice
      # d dimensional feature vector for chosen arm
      Xa             <- context$X[,arm]
      # weights of active context
      weight         <- Xa %*% self$weights
      # assign rewards for active context with weighted probs
      rewards        <- as.double(weight > runif(self$k))
      optimal_arm    <- which.max(rewards)
      reward  <- list(
        reward                   = rewards[arm],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)

horizon <- 100L
simulations <- 50L

weights <- matrix(c(0.8, 0.1, 0.1,
                    0.1, 0.8, 0.1,
                    0.1, 0.1, 0.8), nrow = 3, ncol = 3, byrow = TRUE)

bandit <- BasicContextualBernoulliPrecachingBandit$new(weights = weights)
agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EGreedy"),
               Agent$new(ContextualEpsilonGreedy$new(0.1), bandit, "cEG"),
               Agent$new(LinUCBDisjointPolicy$new(0.6), bandit, "LinUCB"))
simulation <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history <- simulation$run()

#par(mfrow = c(1, 3), mar = c(2, 4, 0.5, 1)) #cex=1.5)
plot(history, type = "cumulative", no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
#plot(history, type = "arms",  limit_agents = c("EGreedy"), no_par = TRUE)
#plot(history, type = "arms",  limit_agents = c("LinUCB"), no_par = TRUE)
#par(mfrow = c(1, 1))
