library("rstan")
UnpooledThompsonPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UnpooledThompsonPolicy",
    n_subjects = NULL,
    initialize = function(n_subjects = 1) {
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list(p = rep(list(list(0,0)),self$n_subjects),
                         n = rep(list(list(0,0)),self$n_subjects))
    },
    get_action = function(t, context) {

      user             <- context$user_context
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        a <- theta$p[[user]][[arm]] * theta$n[[user]][[arm]]
        b <- theta$n[[user]][[arm]] - a
        if (a == 0 || b == 0) {
          expected_rewards[arm] = rbeta(1,1,1)
        } else {
          expected_rewards[arm] = rbeta(1,a,b)
        }
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward
      inc(self$theta$n[[user]][[arm]]) <- 1
      inc(self$theta$p[[user]][[arm]]) <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      self$theta
    }
  )
)

PooledThompsonPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PooledThompsonPolicy",
    n_subjects = NULL,
    initialize = function() {
      super$initialize()
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('N' = 0, 'P' = 0)
    },
    get_action = function(t, context) {
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        a <- self$theta$P[[arm]] * self$theta$N[[arm]]
        b <- self$theta$N[[arm]] - a
        if (a == 0 || b == 0) {
          expected_rewards[arm] = rbeta(1,1,1)
        } else {
          expected_rewards[arm] = rbeta(1,a,b)
        }
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$N[[arm]]) <- 1
      inc(self$theta$P[[arm]]) <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PartiallyPooledThompsonPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    theta_a = NULL,
    theta_b = NULL,
    theta_samples_a = NULL,
    theta_samples_b = NULL,
    stan_model = NULL,
    warm_up = NULL,
    iter = NULL,
    class_name = "PartiallyPooledThompsonPolicy",
    initialize = function(n_subjects = 1,
                          stan_model,
                          warm_up = 10,
                          iter = 20) {
      super$initialize()
      self$n_subjects = n_subjects
      self$stan_model = stan_model
      self$warm_up = warm_up
      self$iter = iter
    },
    set_parameters = function(context_params) {
      self$theta <- list(n = list(rep(0,self$n_subjects),rep(0,self$n_subjects)),  # TODO: make this into k-arms 0, not 0,0
                         l = list(rep(0,self$n_subjects),rep(0,self$n_subjects)))  # TODO: make this into k-arms 0, not 0,0
      self$theta_to_arms <- list('N' = 0)
      n_subjects <- self$n_subjects
      n <- self$theta$n[[1]]
      l <- self$theta$l[[1]]
      capture.output(fit_a <- rstan::sampling(self$stan_model,
                      data = c("n_subjects", "n", "l"),
                      iter = self$iter,
                      warmup = self$warm_up,
                      refresh = 0,
                      chains = 1))
      self$theta_a <- rstan::summary(fit_a, pars = c("theta"))$summary[, "mean"]
      n_subjects <- self$n_subjects
      n <- self$theta$n[[2]]
      l <- self$theta$l[[2]]
      capture.output(fit_b <- rstan::sampling(self$stan_model,
                        data = c("n_subjects", "n", "l"),
                        iter = self$iter,
                        warmup = self$warm_up,
                        refresh = 0,
                        chains = 1))
      self$theta_b <- summary(fit_b, pars = c("theta"))$summary[, "mean"]
      self$theta_samples_a <- extract(fit_a, pars = c("theta"))$theta
      self$theta_samples_b <- extract(fit_b, pars = c("theta"))$theta
    },
    get_action = function(t, context) {
      userid <- context$user_context
      if (self$theta_samples_a[theta$n[[1]][userid] %% 10 + 1, userid] > self$theta_samples_b[theta$n[[2]][userid] %% 10 + 1, userid]) {
        action$choice = 1
      } else {
        action$choice = 2
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm      <- action$choice
      userid   <- context$user_context
      reward   <- reward$reward
      if (arm == 1) {
        self$theta$n[[1]][userid] <- self$theta$n[[1]][userid] + 1
        self$theta$l[[1]][userid] <- self$theta$l[[1]][userid] + reward
        self$theta$N[[1]] <- self$theta$N[[1]] + 1
        if (self$theta$N[[1]] %% 10 == 0) {
          init_val_a <- list(list(theta = self$theta_a))
          n_subjects <- self$n_subjects
          n <- self$theta$n[[1]]
          l <- self$theta$l[[1]]
          capture.output(fit_a <- rstan::sampling(self$stan_model,
                            data = c("n_subjects", "n", "l"),
                            iter = self$iter ,
                            init = init_val_a,
                            warmup = self$warm_up,
                            refresh = 0,
                            chains = 1))
          self$theta_a <- rstan::summary(fit_a, pars = c("theta"))$summary[, "mean"]
          self$theta_samples_a <- rstan::extract(fit_a, pars = c("theta"))$theta
        }
      } else {
        self$theta$n[[2]][userid] <- self$theta$n[[2]][userid] + 1
        self$theta$l[[2]][userid] <- self$theta$l[[2]][userid] + reward
        self$theta$N[[2]] <- self$theta$N[[2]] + 1
        if (self$theta$N[[2]] %% 10 == 0) {
          init_val_b <-
            list(list(
              theta = self$theta_b
            ))
          n_subjects <- self$n_subjects
          n <- self$theta$n[[2]]
          l <- self$theta$l[[2]]
          capture.output(fit_b <- rstan::sampling(self$stan_model,
                            data = c("n_subjects", "n", "l"),
                            iter = self$iter ,
                            init = init_val_b,
                            warmup = self$warm_up,
                            refresh = 0,
                            chains = 1))
          self$theta_b <- rstan::summary(fit_b, pars = c("theta"))$summary[, "mean"]
          self$theta_samples_b <- rstan::extract(fit_b, pars = c("theta"))$theta
        }
      }
      self$theta
    }
  )
)
