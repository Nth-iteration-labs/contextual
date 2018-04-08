UnpooledThompsonPolicy <- R6::R6Class(
  "UnpooledThompsonPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(n_subjects = 1, name = "UnpooledThompson") {
      super$initialize(name)
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list(p = rep(list(list(0,0)),self$n_subjects),
                         n = rep(list(list(0,0)),self$n_subjects))
    },
    get_action = function(context, t) {
      user             <- context$user_context
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        a <- theta$p[[user]][[arm]] * theta$n[[user]][[arm]]
        b <- theta$n[[user]][[arm]] - a
        if (a == 0 || b == 0) {
          expected_rewards[[arm]] = rbeta(1,1,1)
        } else {
          expected_rewards[[arm]] = rbeta(1,a,b)
        }
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(theta$n[[user]][[arm]]) <- 1
      inc(theta$p[[user]][[arm]]) <- (reward - theta$p[[user]][[arm]]) / theta$n[[user]][[arm]]
      theta
    }
  )
)

PooledThompsonPolicy <- R6::R6Class(
  "PooledThompsonPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(name = "PooledThompson") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$theta_to_arms <- list('n_zero' = 0, 'p_zero' = 0)
    },
    get_action = function(context, t) {
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        a <- theta$p_zero[[arm]] * theta$n_zero[[arm]]
        b <- theta$n_zero[[arm]] - a
        if (a == 0 || b == 0) {
          expected_rewards[[arm]] = rbeta(1,1,1)
        } else {
          expected_rewards[[arm]] = rbeta(1,a,b)
        }
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      inc(theta$n_zero[[arm]]) <- 1
      inc(theta$p_zero[[arm]]) <- (reward - theta$p_zero[[arm]]) / theta$n_zero[[arm]]
      theta
    }
  )
)
