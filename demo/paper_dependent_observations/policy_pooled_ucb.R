UnpooledUCBPolicy <- R6::R6Class(
  "UnpooledUCBPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    initialize = function(n_subjects = 1, name = "PooledUCB") {
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list('n_total' = rep(0,self$n_subjects), n = rep(list(list(0,0)),self$n_subjects), p = rep(list(list(0,0)), self$n_subjects))
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < self$k) {
        for (arm in 1:self$k) {
          if (self$theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        expected_rewards[[arm]] <- self$theta$p[[user]][[arm]] +
          sqrt(2*log(self$theta$n_total[[user]])/self$theta$n[[user]][[arm]])
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(self$theta$n_total[[user]])  <- 1
      inc(self$theta$n[[user]][[arm]]) <- 1
      inc(self$theta$p[[user]][[arm]]) <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      self$theta
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
      super$initialize()
    },
    set_parameters = function() {
      self$theta_to_arms <- list('P' = 0, 'N' = 0)
      self$theta <- list('N_total' = 0)
    },
    get_action = function(t, context) {
      if (self$theta$N_total < self$k) {
        for (arm in 1:self$k) {
          if (self$theta$N[[arm]] == 0) {
             action$choice <- arm
             return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      for (arm in 1:self$k) {
        expected_rewards[[arm]] <- self$theta$P[[arm]] +
          sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$N_total)  <- 1
      inc(self$theta$N[[arm]]) <- 1
      inc(self$theta$P[[arm]]) <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
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
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list('N_total' = 0, 'n_total' = rep(0,self$n_subjects), n = rep(list(list(0,0)),self$n_subjects), p = rep(list(list(0,0)), self$n_subjects))
      self$theta_to_arms <- list('P' = 0, 'N' = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < self$k) {
        for (arm in 1:self$k) {
          if (self$theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, self$k)
      beta = 1/sqrt(self$theta$n_total[[user]])
      for (arm in 1:self$k) {
        p_mean <- self$theta$P[[arm]] + sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])
        p_choice <- self$theta$p[[user]][[arm]] + sqrt(2*log(self$theta$n_total[[user]])/self$theta$n[[user]][[arm]])
        p_hat = (beta * p_mean + (1-beta) * p_choice)
        expected_rewards[[arm]] = p_hat
      }
      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(self$theta$n_total[[user]])  <- 1
      inc(self$theta$n[[user]][[arm]]) <- 1
      inc(self$theta$p[[user]][[arm]]) <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      inc(self$theta$N_total)     <- 1
      inc(self$theta$N[[arm]])    <- 1
      inc(self$theta$P[[arm]])    <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)
