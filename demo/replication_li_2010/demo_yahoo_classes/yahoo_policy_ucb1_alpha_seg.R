#' @export
YahooUCB1AlphaSegPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    cluster = NULL,
    class_name = "YahooUCB1AlphaSegPolicy",
    initialize = function(alpha) {
      super$initialize()
      self$alpha                  <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms          <- list('n' = rep(0,5), 'mean' = rep(0,5))
    },
    get_action = function(t, context) {
      # find the feature on which a user scores highest - that is this user's cluster
      self$cluster                <- which.max(head(context$X[context$d_disjoint,1],-1))
      local_arms                  <- context$arms
      for (arm in seq_along(local_arms)) {
        if(self$theta$n[[local_arms[arm]]][self$cluster] == 0) {
          action$choice             <- local_arms[arm]
          return(action)
        }
      }
      expected_rewards            <- rep(0.0, length(local_arms))
      for (arm in seq_along(local_arms)) {
        variance                  <- self$alpha / sqrt( self$theta$n[[local_arms[arm]]][self$cluster] )
        expected_rewards[arm]     <- self$theta$mean[[local_arms[arm]]][self$cluster] + variance
      }
      action$choice               <- local_arms[max_in(expected_rewards)]
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
