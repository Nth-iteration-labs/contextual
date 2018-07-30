UnpooledEgreedyPolicy <- R6::R6Class(
  "UnpooledEgreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1, name = "PooledEGreedy") {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(k, d, u, s) {
      self$theta <- list(n = rep(list(as.list(rep(0, context$k))),self$n_subjects), mu = rep(list(as.list(rep(0, context$k))), self$n_subjects))
    },
    get_action = function(t, context) {
      if (runif(1) > epsilon) {
        action$choice <- which.max(unlist(self$theta$mu[[context$user_context]], FALSE, FALSE))
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(self$theta$n[[user]][[arm]])    <- 1
      inc(self$theta$mu[[user]][[arm]])   <- (reward - self$theta$mu[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      self$theta
    }
  )
)

PooledEgreedyPolicy <- R6::R6Class(
  "PooledEgreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, name = "PooledEGreedy") {
      super$initialize()
      self$epsilon <- epsilon
    },
    set_parameters = function(k, d, u, s) {
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(t, context) {
      if (runif(1) > epsilon) {
        action$choice <- max_in(self$theta$MU)
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$N[[arm]])    <- 1
      inc(self$theta$MU[[arm]]) <- (reward - self$theta$MU[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PartiallyPooledEgreedyPolicy <- R6::R6Class(
  "PartiallyPooledEgreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1, name = "PartiallyPooledEgreedy") {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(k, d, u, s) {
      # here, n 1/1  because of beta...
      self$theta         <- list(n = rep(list(as.list(rep(1, context$k))),self$n_subjects), mu = rep(list(as.list(rep(0, context$k))),self$n_subjects))
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        beta <- 1 / sqrt(sum_of(self$theta$n[[user]]))
        p_hat  <- beta * unlist(self$theta$MU) + (1 - beta) * unlist(self$theta$mu[[user]])
        action$choice <- max_in(p_hat)
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(self$theta$n[[user]][[arm]])     <- 1
      inc(self$theta$mu[[user]][[arm]])    <- (reward - self$theta$mu[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      inc(self$theta$N[[arm]])             <- 1
      inc(self$theta$MU[[arm]])            <- (reward - self$theta$MU[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)
