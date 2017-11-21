library(R6)

#' @export
LinUCBPolicy <- R6Class(
  "LinUCBPolicy",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    alpha = 0.1,
    initialize = function(alpha = 0.1) {
      self$alpha = alpha
    },
    get_action = function(agent,context) {
      memory = agent$get_memory()
      context_vector = t(as.matrix(context))
      k = length(memory[['theta']])
      expected_rewards_vector = vector(mode = "numeric", length = bandit$k)
      for (arm in 1:k) {

        A          = memory[['theta']][[arm]][['A']]
        b          = as.matrix(memory[['theta']][[arm]][['b']])
        A_inv      = solve(A)
        theta_hat  = A_inv %*% b

        mean =  t(context_vector) %*% theta_hat
        var  =  sqrt( ( t(context_vector) %*% A_inv ) %*% context_vector )

        expected_reward_for_arm = mean + (self$alpha * var)
        expected_rewards_vector[arm] = expected_reward_for_arm

      }
      return(index_of_max(expected_rewards_vector))
    }

  )
)
