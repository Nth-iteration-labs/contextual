UnpooledUCBPolicy <- R6::R6Class(
  "UnpooledUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(n_subjects = 1, name = "PooledUCB") {
      super$initialize(name)
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list('n_total' = rep(0,self$n_subjects), n = rep(list(list(0,0)),self$n_subjects), p = rep(list(list(0,0)), self$n_subjects))
    },
    get_action = function(context, t) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < self$k) {
        for (arm in 1:self$k) {
          if (theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        expected_rewards[[arm]] <- theta$p[[user]][[arm]] +
          sqrt(2*log(theta$n_total[[user]])/theta$n[[user]][[arm]])
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(theta$n_total[[user]])  <- 1
      inc(theta$n[[user]][[arm]]) <- 1
      inc(theta$p[[user]][[arm]]) <- (reward - theta$p[[user]][[arm]]) / theta$n[[user]][[arm]]
      theta
    }
  )
)

PooledUCBPolicy <- R6::R6Class(
  "PooledUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(name = "PooledUCB") {
      super$initialize(name)
    },
    set_parameters = function() {
      self$theta_to_arms <- list('p_zero' = 0, 'n_zero' = 0)
      self$theta <- list('n_total_zero' = 0)
    },
    get_action = function(context, t) {
      if (self$theta$n_total_zero < self$k) {
        for (arm in 1:self$k) {
          if (theta$n_zero[[arm]] == 0) {
             action$choice <- arm
             return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        expected_rewards[[arm]] <- theta$p_zero[[arm]] +
          sqrt(2*log(theta$n_total_zero)/theta$n_zero[[arm]])
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      inc(theta$n_total_zero)  <- 1
      inc(theta$n_zero[[arm]]) <- 1
      inc(theta$p_zero[[arm]]) <- (reward - theta$p_zero[[arm]]) / theta$n_zero[[arm]]
      theta
    }
  )
)

PartiallyPooledUCBPolicy <- R6::R6Class(
  "PartiallyPooledUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(n_subjects = 1, name = "PartiallyPooledUCB") {
      super$initialize(name)
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list('n_total_zero' = 0, 'n_total' = rep(0,self$n_subjects), n = rep(list(list(0,0)),self$n_subjects), p = rep(list(list(0,0)), self$n_subjects))
      self$theta_to_arms <- list('p_zero' = 0, 'n_zero' = 0)
    },
    get_action = function(context, t) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < self$k) {
        for (arm in 1:self$k) {
          if (theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      beta = 1/sqrt(self$theta$n_total[[user]])
      for (arm in 1:self$k) {
        p_mean <- theta$p_zero[[arm]] + sqrt(2*log(theta$n_total_zero)/theta$n_zero[[arm]])
        p_choice <- theta$p[[user]][[arm]] + sqrt(2*log(theta$n_total[[user]])/theta$n[[user]][[arm]])
        p_hat = (beta * p_mean + (1-beta) * p_choice)
        expected_rewards[[arm]] = p_hat
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(theta$n_total[[user]])  <- 1
      inc(theta$n[[user]][[arm]]) <- 1
      inc(theta$p[[user]][[arm]]) <- (reward - theta$p[[user]][[arm]]) / theta$n[[user]][[arm]]
      inc(theta$n_total_zero)     <- 1
      inc(theta$n_zero[[arm]])    <- 1
      inc(theta$p_zero[[arm]])    <- (reward - theta$p_zero[[arm]]) / theta$n_zero[[arm]]
      theta
    }
  )
)
