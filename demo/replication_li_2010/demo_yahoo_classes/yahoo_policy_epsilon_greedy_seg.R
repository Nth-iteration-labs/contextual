YahooEpsilonGreedySegPolicy          <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    cluster = NULL,
    class_name = "YahooEpsilonGreedySegPolicy",
    initialize = function(epsilon = 0.1) {
      super$initialize()
      self$epsilon                <- epsilon
    },
    set_parameters = function() {
      self$theta_to_arms          <- list('n' = rep(0,5), 'mean' = rep(0,5))
    },
    get_action = function(t, context) {
      local_arms                  <- context$arms
      if (runif(1) > self$epsilon) {
        # find the feature on which a user scores highest - that is this user's cluster
        self$cluster              <- which.max(head(context$X[context$d_shared,1],-1))
        expected_rewards          <- rep(0.0, length(local_arms))
        for (arm in seq_along(local_arms)) {
          expected_rewards[arm]   <- self$theta$mean[[local_arms[arm]]][self$cluster]
        }
        action$choice             <- local_arms[max_in(expected_rewards)]
      } else {
        action$choice             <- sample(local_arms, 1)
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                                       <- action$choice
      reward                                    <- reward$reward
      self$theta$n[[arm]][self$cluster]         <- self$theta$n[[arm]][self$cluster] + 1
      self$theta$mean[[arm]][self$cluster]      <- self$theta$mean[[arm]][self$cluster] +
                                                   (reward - self$theta$mean[[arm]][self$cluster]) /
                                                   self$theta$n[[arm]][self$cluster]
      self$theta
    }
  )
)
