#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

# bandit with just two arms, one feature representing one out of n_subjects users
BernoulliBandit <- R6::R6Class(
  "BernoulliBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    n_subjects     = NULL, # number of subjects in trial
    probs_arm_one  = NULL, # n_subjects random deviates
    probs_arm_two  = NULL, # n_subjects random deviates
    precaching     = FALSE,
    initialize  = function(n_subjects, arm_one_shape = c(10,10), arm_two_shape = c(10,10), start_seed = 42) {
      set.seed(start_seed)
      self$k                              <- 2              # two armed bandit
      self$d                              <- 1              # one context feature, which user
      self$n_subjects                     <- n_subjects     # number of subjects..
      self$probs_arm_one                  <- rbeta(n_subjects, arm_one_shape[1], arm_one_shape[2])
      self$probs_arm_two                  <- rbeta(n_subjects, arm_two_shape[1], arm_two_shape[2])
    },
    get_context = function(t) {
      contextlist <- list(
        k = self$k,
        d = self$d,
        user_context = sample(1:self$n_subjects, 1)
      )
      contextlist
    },
    do_action = function(context, action, t) {
      subject              <- context$user_context
      rewardlist           <- list()
      rewardlist$opimal    <- rbinom(1, 1, max(self$probs_arm_one[subject], self$probs_arm_two[subject]))
      if (action$choice == 1)
        rewardlist$reward  <- rbinom(1, 1, self$probs_arm_one[subject])
      else
        rewardlist$reward  <- rbinom(1, 1, self$probs_arm_two[subject])
      rewardlist
    }
  )
)

# fully pooled if n_subjects = 1
UnpooledEgreedy <- R6::R6Class(
  "UnpooledEgreedy",
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
      self$theta <- list(n=rep(list(list(0,0)),self$n_subjects), mu=rep(list(list(0,0)),self$n_subjects))
    },
    get_action = function(context, t) {
      if (runif(1) > epsilon) {
        action$choice <- which.max(unlist(theta$mu[[context$user_context]]))
        action$choice
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

PooledEgreedy <- R6::R6Class(
  "UnpooledEgreedy",
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
      self$theta_to_arms <- list('n_zero' = 0, 'mu_zero' = 0)
    },
    get_action = function(context, t) {
      if (runif(1) > epsilon) {
        action$choice <- max_in(theta$mu_zero)
        action$choice
      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm <- action$choice
      reward <- reward$reward
      inc(theta$n_zero[[arm]])    <- 1
      inc(theta$mu_zero[[arm]]) <- (reward - theta$mu_zero[[arm]]) / theta$n_zero[[arm]]
      theta
    }
  )
)

PartiallyPooledEgreedy <- R6::R6Class(
  "PartiallyPooledEgreedy",
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
      self$theta_to_arms <- list('n_zero' = 0, 'mu_zero' = 0)
    },
    get_action = function(context, t) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        # Calculate p_a_i_hat and p_b_i_hat
        beta <- 1/sqrt(theta$n[[user]][[1]] + theta$n[[user]][[2]])
        p_a_mean <- theta$mu_zero[[1]]
        p_b_mean <- theta$mu_zero[[2]]
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

      inc(theta$n_zero[[arm]])        <- 1
      inc(theta$mu_zero[[arm]])       <- (reward - theta$mu_zero[[arm]]) / theta$n_zero[[arm]]

      theta
    }
  )
)

horizon     <- 5000
simulations <- 100
n_subjects  <- 500

bandit             <- BernoulliBandit$new(n_subjects = n_subjects, arm_one_shape = c(10,2), arm_two_shape = c(2,10))

agents             <- list( Agent$new(PartiallyPooledEgreedy$new(epsilon = 0.1, n_subjects = n_subjects, name = "Partial"), bandit),
                            Agent$new(UnpooledEgreedy$new(epsilon = 0.1, n_subjects = n_subjects, name = "Unpooled"), bandit),
                            Agent$new(PooledEgreedy$new(epsilon = 0.1, name = "Pooled"), bandit))

history            <- Simulator$new(agents = agents,
                                    horizon = horizon,
                                    simulations = simulations,
                                    save_theta = FALSE,
                                    do_parallel = TRUE)$run()

plot(history, type = "grid")
plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "arms")
