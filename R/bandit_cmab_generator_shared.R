#' @export
ContextualGeneratorSharedBandit <- R6::R6Class(
  "ContextualGeneratorSharedBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    class_name = "ContextualGeneratorSharedBandit",
    precaching = FALSE,
    initialize  = function(k, d) {
      self$k       <- k
      self$d       <- d
    },
    post_initialization = function() {
      self$betas   <- rnorm(self$d,0,1)
    },
    get_context = function(t) {

      X <- matrix(runif(self$d*self$k, 0, 1), self$d, self$k)

      context_list <- list(
        k = self$k,
        d = self$d,
        X = X
      )
      context_list
    },
    get_reward = function(t, context, action) {

      reward <- 2*rbinom(1,1,1/(1+exp(-self$betas%*%context$X[,action$choice])))/2
      rewardlist <- list(
        reward                   = reward,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)
