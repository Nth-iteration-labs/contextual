#' @export
ContextualLinearBandit <- R6::R6Class(
  "ContextualLinearBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    sigma   = NULL,
    binary  = NULL,
    class_name = "ContextualLinearBandit",
    precaching = FALSE,
    initialize  = function(k, d, sigma = 0.1, binary_rewards = FALSE) {
      self$k                                    <- k
      self$d                                    <- d
      self$sigma                                <- sigma
      self$binary                               <- binary_rewards
    },
    post_initialization = function() {
      self$betas                                <- matrix(runif(self$d*self$k, -1, 1), self$d, self$k)
      self$betas                                <- self$betas / norm(self$betas, type = "2")
    },
    get_context = function(t) {
      context                                   <- rnorm(self$d)
      reward_vector                             <- context %*% self$betas
      context                                   <- matrix(context, self$d,self$k)
      reward_vector                             <- reward_vector + rnorm(self$k, sd = self$sigma)

      if (isTRUE(self$binary)) {
        self$rewards                            <- rep(0,self$k)
        self$rewards[which.max(reward_vector)]  <- 1
      } else {
        self$rewards                            <- reward_vector
      }
      context_list <- list(
        k = self$k,
        d = self$d,
        X = context
      )
      context_list
    },
    get_reward = function(t, context_common, action) {
      rewardlist <- list(
        reward                   = self$rewards[action$choice],
        optimal_reward_value     = self$rewards[which.max(self$rewards)]
      )
      rewardlist
    }
  )
)

#' ContextualLinearBandit
#'
#' @name ContextualLinearBandit
#'
NULL
