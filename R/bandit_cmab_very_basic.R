# Samples data from linearly parameterized arms.
#
# The reward for context X and arm j is given by X^T beta_j, for some latent
# set of parameters {beta_j : j = 1, ..., k}. The beta's are sampled uniformly
# at random, the contexts are Gaussian, and sigma-noise is added to the rewards.
#
# Args:
# num_contexts: Number of contexts to sample.
# dim_context: Dimension of the contexts.
# num_actions: Number of arms for the multi-armed bandit.
# sigma: Standard deviation of the additive noise. Set to zero for no noise.
#
# Returns:
# data: A [n, d+k] numpy array with the data.
# betas: Latent parameters that determine expected reward for each arm.
# opt: (optimal_rewards, optimal_actions) for all contexts.

#' @export
VeryBasicContextualBandit <- R6::R6Class(
  "VeryBasicContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    sigma   = NULL,
    binary  = NULL,
    class_name = "VeryBasicContextualBandit",
    precaching = FALSE,
    initialize  = function(k, d, sigma = 0.1, binary_rewards = FALSE) {
      self$k                                    <- k
      self$d                                    <- d
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
      best_action <- which.max(colSums(context$X))
      rewardretrun <- 0
      if (best_action==action$choice) rewardretrun = 1
      rewardlist <- list(
        reward                   = rewardretrun,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)
