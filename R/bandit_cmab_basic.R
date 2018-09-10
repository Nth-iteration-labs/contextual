#' @export
ContextualBasicBandit <- R6::R6Class(
  "ContextualBasicBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    class_name = "ContextualBasicBandit",
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
      reward <- rbinom(1,1,1/(1+exp(-self$betas%*%context$X[,action$choice])))
      rewardlist <- list(
        reward                   = reward,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)

#' ContextualBasicBandit
#'
#' @name ContextualBasicBandit
#'
NULL
