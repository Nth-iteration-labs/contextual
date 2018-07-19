#' @export
YahooUCB1AlphaPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "YahooUCB1AlphaPolicy",
    initialize = function(alpha) {
      super$initialize()
      self$alpha                  <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms <- list('n' = 0, 'mean' = 0)
    },
    get_action = function(t, context) {
      local_arms       <- context$arms
      n_zero_arms      <- which(self$theta$n[local_arms] == 0)
      if (length(n_zero_arms) > 0) {
        action$choice <- local_arms[sample_one_of(n_zero_arms)]
        return(action)
      }
      expected_rewards <- rep(0.0, length(context$arms))
      for (arm in seq_along(local_arms)) {
        # usb1 variance as in Li 2010 paper
        variance <- self$alpha / sqrt( self$theta$n[[local_arms[arm]]] )
        expected_rewards[arm] <- self$theta$mean[[local_arms[arm]]] + variance
      }
      action$choice <- local_arms[max_in(expected_rewards)]
      action
    },
    set_reward = function(t, context, action, reward) {

      arm                         <- action$choice
      reward                      <- reward$reward
      self$theta$n[[arm]]         <- self$theta$n[[arm]] + 1
      self$theta$mean[[arm]]      <- self$theta$mean[[arm]] + (reward - self$theta$mean[[arm]]) / self$theta$n[[arm]]

      self$theta
    }
  )
)
