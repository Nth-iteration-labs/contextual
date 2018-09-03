YahooLinUCBDisjointPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "YahooLinUCBDisjointPolicy",
    initialize = function(alpha = 0.2) {
      super$initialize()
      self$alpha  <- alpha
    },
    set_parameters = function(context_params) {
      ul <- length(context_params$unique)

      self$theta_to_arms <- list( 'A' = diag(1,ul,ul), 'b' = rep(0,ul),
                                  'A_inv' = solve(diag(1,ul,ul)))
    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, length(context$arms))
      local_arms       <- context$arms
      for (arm in seq_along(local_arms)) {

        x            <- context$X[context$unique,arm]
        A            <- self$theta$A[[local_arms[arm]]]
        A_inv        <- self$theta$A_inv[[local_arms[arm]]]
        b            <- self$theta$b[[local_arms[arm]]]
        theta_hat    <- A_inv %*% b
        mean         <- x %*% theta_hat
        sd           <- sqrt(tcrossprod(x %*% A_inv, x))
        expected_rewards[arm] <- mean + self$alpha * sd
      }
      action$choice  <- context$arms[max_in(expected_rewards)]

      action
    },
    set_reward = function(t, context, action, reward) {

      arm                       <- action$choice
      arm_index                 <- which(context$arms == arm)
      reward                    <- reward$reward
      x                         <- context$X[context$unique,arm_index]
      A_inv                     <- self$theta$A_inv[[arm]]
      self$theta$A_inv[[arm]]   <- sherman_morrisson(self$theta$A_inv[[arm]],x)
      self$theta$A[[arm]]       <- self$theta$A[[arm]] + outer(x, x)
      self$theta$b[[arm]]       <- self$theta$b[[arm]] + reward * x

      self$theta
    }
  )
)
