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
PartiallyPooledThompsonPolicy <- R6::R6Class(
  "PartiallyPooledThompsonPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    n_subjects = NULL,
    theta_a = NULL,
    kappa_a = NULL,
    phi_a = NULL,
    theta_b = NULL,
    kappa_b = NULL,
    phi_b = NULL,
    theta_samples_a = NULL,
    phi_samples_a = NULL,       # why? deleted.
    kappa_samples_a = NULL,     # why? deleted.
    theta_samples_b = NULL,
    phi_samples_b = NULL,       # why? deleted.
    kappa_samples_b = NULL,     # why? deleted.
    fit_a = NULL,
    fit_b = NULL,
    initialize = function(n_subjects = 1, name = "PartiallyPooledThompsonPolicy") {
      super$initialize(name)
      self$n_subjects = n_subjects
    },
    set_parameters = function() {
      self$theta <- list(n = list(rep(0,self$n_subjects),rep(0,self$n_subjects)),  # TODO: make this into k-arms 0, not 0,0
                         l = list(rep(0,self$n_subjects),rep(0,self$n_subjects)))  # TODO: make this into k-arms 0, not 0,0
      self$theta_to_arms <- list('n_zero' = 0)
      ## theta, phi, kappa (for computing posterior)
      ## For this we just run Stan once so that we can use the parameters in a loop later
      n_subjects <- self$n_subjects
      n <- theta$n[[1]]
      l <- theta$l[[1]]
      self$fit_a <-
        stan(
          "beta_binom_hier_a.stan",
          data = c("n_subjects", "n", "l"),
          iter = 20,
          warmup = 10,
          chains = 1,
          seed = 1234  ### TODO: use main seed here!
        )
      self$theta_a <- summary(self$fit_a, pars = c("theta"))$summary[, "mean"]
      self$kappa_a <- summary(self$fit_a, pars = c("kappa"))$summary[, "mean"]
      self$phi_a <- summary(self$fit_a, pars = c("phi"))$summary[, "mean"]
      n_subjects <- self$n_subjects
      n <- theta$n[[2]]
      l <- theta$l[[2]]
      self$fit_b <-
        stan(
          "beta_binom_hier_b.stan",
          data = c("n_subjects", "n", "l"),
          iter = 20,
          warmup = 10,
          chains = 1,
          seed = 1234  ### TODO: use main seed here!
        )
      self$theta_b <- summary(self$fit_b, pars = c("theta"))$summary[, "mean"]
      self$kappa_b <- summary(self$fit_b, pars = c("kappa"))$summary[, "mean"]
      self$phi_b <- summary(self$fit_b, pars = c("phi"))$summary[, "mean"]

      # Furthermore, we need to keep a list theta_samples

      self$theta_samples_a <- extract(self$fit_a, pars = c("theta"))$theta
      self$theta_samples_b <- extract(self$fit_b, pars = c("theta"))$theta
    },
    get_action = function(context, t) {
      userid <- context$user_context
      if (self$theta_samples_a[theta$n[[1]][userid] %% 10 + 1, userid] > self$theta_samples_b[theta$n[[2]][userid] %% 10 + 1, userid]) {
        action$choice = 1
      } else {
        action$choice = 2
      }
      action
    },
    set_reward = function(context, action, reward, t) {
      arm      <- action$choice
      userid   <- context$user_context
      reward   <- reward$reward

      # First update user parameters
      if (arm == 1) {
        theta$n[[1]][userid] <- theta$n[[1]][userid] + 1
        theta$l[[1]][userid] <- theta$l[[1]][userid] + reward
        theta$n_zero[[1]] <- theta$n_zero[[1]] + 1
        if (theta$n_zero[[1]] %% 10 == 0) {
          init_val_a <-
            list(list(
              theta = self$theta_a,
              kappa = self$kappa_a,
              phi = self$phi_a
            ))
          n_subjects <- self$n_subjects
          n <- theta$n[[1]]
          l <- theta$l[[1]]
          self$fit_a <-
            stan(
              "beta_binom_hier_a.stan",
              data = c("n_subjects", "n", "l"),
              iter = 20,
              warmup = 10,
              init = init_val_a,
              chains = 1,
              seed = 1234
            )
          self$theta_a <- summary(self$fit_a, pars = c("theta"))$summary[, "mean"]
          self$kappa_a <- summary(self$fit_a, pars = c("kappa"))$summary[, "mean"]
          self$phi_a <- summary(self$fit_a, pars = c("phi"))$summary[, "mean"]
          self$theta_samples_a <- extract(self$fit_a, pars = c("theta"))$theta
        }
      } else {
        # arm b
        theta$n[[2]][userid] <- theta$n[[2]][userid] + 1
        theta$l[[2]][userid] <- theta$l[[2]][userid] + reward
        theta$n_zero[[2]] <- theta$n_zero[[2]] + 1
        if (theta$n_zero[[2]] %% 10 == 0) {
          init_val_b <-
            list(list(
              theta = self$theta_b,
              kappa = self$kappa_b,
              phi = self$phi_b
            ))
          n_subjects <- self$n_subjects
          n <- theta$n[[2]]
          l <- theta$l[[2]]
          self$fit_b <-
            stan(
              "beta_binom_hier_b.stan",
              data = c("n_subjects", "n", "l"),
              iter = 20,
              warmup = 10,
              init = init_val_b,
              chains = 1,
              seed = 1234
            )
          self$theta_b <- summary(self$fit_b, pars = c("theta"))$summary[, "mean"]
          self$kappa_b <- summary(self$fit_b, pars = c("kappa"))$summary[, "mean"]
          self$phi_b <- summary(self$fit_b, pars = c("phi"))$summary[, "mean"]
          self$theta_samples_b <- extract(self$fit_b, pars = c("theta"))$theta
          self$phi_samples_b <- extract(self$fit_b, pars = c("phi"))$phi
          self$kappa_samples_b <- extract(self$fit_b, pars = c("kappa"))$kappa
        }
      }
      theta
    }
  )
)
