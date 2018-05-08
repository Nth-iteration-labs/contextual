UnpooledEgreedyPolicy <- R6::R6Class(
  "UnpooledEgreedyPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1, name = "PooledEGreedy") {
      super$initialize(name)
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      self$theta <- list(n = rep(list(as.list(rep(0, self$k))),self$n_subjects), mu = rep(list(as.list(rep(0, self$k))), self$n_subjects))
    },
    get_action = function(context, t) {
      if (runif(1) > epsilon) {
        action$choice <- which.max(unlist(theta$mu[[context$user_context]]))
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(theta$n[[user]][[arm]])    <- 1
      inc(theta$mu[[user]][[arm]])   <- (reward - theta$mu[[user]][[arm]]) / theta$n[[user]][[arm]]
      theta
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
      super$initialize(name)
      self$epsilon <- epsilon
    },
    set_parameters = function() {
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(context, t) {
      if (runif(1) > epsilon) {
        action$choice <- max_in(theta$MU)
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      inc(theta$N[[arm]])    <- 1
      inc(theta$MU[[arm]]) <- (reward - theta$MU[[arm]]) / theta$N[[arm]]
      theta
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
      super$initialize(name)
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function() {
      # here, n 1/1  because of beta...
      self$theta         <- list(n = rep(list(as.list(rep(1, self$k))),self$n_subjects), mu = rep(list(as.list(rep(0, self$k))),self$n_subjects))
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(context, t) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        beta <- 1 / sqrt(sum_of(theta$n[[user]]))
        p_hat  <- beta * unlist(theta$MU) + (1 - beta) * unlist(theta$mu[[user]])
        action$choice <- max_in(p_hat)
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(theta$n[[user]][[arm]])     <- 1
      inc(theta$mu[[user]][[arm]])    <- (reward - theta$mu[[user]][[arm]]) / theta$n[[user]][[arm]]
      inc(theta$N[[arm]])             <- 1
      inc(theta$MU[[arm]])            <- (reward - theta$MU[[arm]]) / theta$N[[arm]]
      theta
    }
  )
)
