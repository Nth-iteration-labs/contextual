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
      self$theta <- list(n = rep(list(list(0,0)),self$n_subjects), mu = rep(list(list(0,0)), self$n_subjects))
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
      self$theta         <- list(n = rep(list(list(1,1)),self$n_subjects), mu = rep(list(list(0,0)),self$n_subjects))
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(context, t) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        # Calculate p_a_i_hat and p_b_i_hat
        beta <- 1/sqrt(theta$n[[user]][[1]] + theta$n[[user]][[2]])
        p_a_mean <- theta$MU[[1]]
        p_b_mean <- theta$MU[[2]]
        p_a_hat  <- beta * p_a_mean + (1 - beta) * theta$mu[[user]][[1]]
        p_b_hat  <- beta * p_b_mean + (1 - beta) * theta$mu[[user]][[2]]
        # Take maximum of the two
        if (p_a_hat > p_b_hat) {
          action$choice <- 1
        } else {
          action$choice <- 2
        }
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
      inc(theta$N[[arm]])        <- 1
      inc(theta$MU[[arm]])       <- (reward - theta$MU[[arm]]) / theta$N[[arm]]
      theta
    }
  )
)
