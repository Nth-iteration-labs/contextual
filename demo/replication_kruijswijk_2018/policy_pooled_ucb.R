UnpooledUCBPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UnpooledUCBPolicy",
    n_subjects = NULL,
    initialize = function(n_subjects = 1, name = "UnpooledUCBPolicy") {
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list("n_total" = rep(0,self$n_subjects),
                         n = rep(list(as.list(rep(0, context_params$k))), self$n_subjects),
                         p = rep(list(as.list(rep(0, context_params$k))), self$n_subjects))
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        expected_rewards[arm] <- self$theta$p[[user]][[arm]] +
          sqrt(2*log(self$theta$n_total[[user]])/self$theta$n[[user]][[arm]])
      }
      action$choice  <- which_max_tied(expected_rewards)
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
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PooledUCBPolicy",
    n_subjects = NULL,
    initialize = function() {
      super$initialize()
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list("P" = 0, "N" = 0)
      self$theta <- list("N_total" = 0)
    },
    get_action = function(t, context) {
      if (self$theta$N_total < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$N[[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        expected_rewards[[arm]] <- self$theta$P[[arm]] +
          sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$N_total)  <- 1
      inc(self$theta$N[[arm]])   <- 1
      inc(self$theta$P[[arm]])   <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PooledUCBPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PooledUCBPolicy",
    n_subjects = NULL,
    initialize = function() {
      super$initialize()
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list("P" = 0, "N" = 0)
      self$theta <- list("N_total" = 0)
    },
    get_action = function(t, context) {
      if (self$theta$N_total < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$N[[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        expected_rewards[[arm]] <- self$theta$P[[arm]] +
          sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm <- action$choice
      reward <- reward$reward
      inc(self$theta$N_total)  <- 1
      inc(self$theta$N[[arm]])   <- 1
      inc(self$theta$P[[arm]])   <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PartiallyPooledUCBPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledUCBPolicy",
    n_subjects = NULL,
    initialize = function(n_subjects = 1) {
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list("N_total" = 0, "n_total" = rep(0,self$n_subjects),
                         n = rep(list(as.list(rep(0, context_params$k))), self$n_subjects),
                         p = rep(list(as.list(rep(0, context_params$k))), self$n_subjects))
      self$theta_to_arms <- list("P" = 0, "N" = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, context$k)
      #beta = 1/sqrt(self$theta$n_total[[user]])
      beta = 2/(2+self$theta$n_total[[user]])
      for (arm in 1:context$k) {
        p_mean <- self$theta$P[[arm]] + sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])
        p_choice <- self$theta$p[[user]][[arm]] + sqrt(2*log(self$theta$n_total[[user]])/self$theta$n[[user]][[arm]])
        p_hat = (beta * p_mean + (1-beta) * p_choice)
        expected_rewards[arm] = p_hat
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                              <- action$choice
      user                             <- context$user_context
      reward                           <- reward$reward
      inc(self$theta$n_total[[user]])  <- 1
      inc(self$theta$n[[user]][[arm]]) <- 1
      inc(self$theta$p[[user]][[arm]]) <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      inc(self$theta$N_total)          <- 1
      inc(self$theta$N[[arm]])         <- 1
      inc(self$theta$P[[arm]])         <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PartiallyPooledUCBPolicyOld <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledUCBPolicyOld",
    n_subjects = NULL,
    initialize = function(n_subjects = 1) {
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list("N_total" = 0, "n_total" = rep(0,self$n_subjects),
                         n = rep(list(as.list(rep(0, context_params$k))), self$n_subjects),
                         p = rep(list(as.list(rep(0, context_params$k))), self$n_subjects))
      self$theta_to_arms <- list("P" = 0, "N" = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$n_total[[user]] < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$n[[user]][[arm]] == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      expected_rewards <- rep(0.0, context$k)
      beta = 1/sqrt(self$theta$n_total[[user]])
      for (arm in 1:context$k) {
        p_mean <- self$theta$P[[arm]] + sqrt(2*log(self$theta$N_total)/self$theta$N[[arm]])/sqrt(self$theta$n[[user]][[arm]])
        p_choice <- self$theta$p[[user]][[arm]] + sqrt(2*log(self$theta$n_total[[user]])/self$theta$n[[user]][[arm]])/sqrt(self$theta$N[[arm]])
        p_hat = (beta * p_mean + (1-beta) * p_choice)
        expected_rewards[arm] = p_hat
      }
      action$choice  <- which_max_tied(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm                              <- action$choice
      user                             <- context$user_context
      reward                           <- reward$reward
      inc(self$theta$n_total[[user]])  <- 1
      inc(self$theta$n[[user]][[arm]]) <- 1
      inc(self$theta$p[[user]][[arm]]) <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]
      inc(self$theta$N_total)          <- 1
      inc(self$theta$N[[arm]])         <- 1
      inc(self$theta$P[[arm]])         <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta
    }
  )
)

PartiallyBBPooledUCBPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledBBUCBPolicy",
    n_subjects = NULL,
    initialize = function(n_subjects = 1) {
      super$initialize()
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list("N_total" = 0, "n_total" = rep(0,self$n_subjects),
                         n  = rep(list(as.list(rep(1, context_params$k))),self$n_subjects),
                         p  = rep(list(as.list(rep(0, context_params$k))),self$n_subjects),
                         ss = rep(list(as.list(rep(0, context_params$k))),self$n_subjects),
                         c  = rep(list(as.list(rep(0, context_params$k))),self$n_subjects))
      self$theta_to_arms <- list("N"  = 1, "P" = 0, "SS" = 0, "C" = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (self$theta$N_total < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$N[[arm]]-1 == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }
      if (self$theta$n_total[[user]] < context$k) {
        for (arm in 1:context$k) {
          if (self$theta$n[[user]][[arm]]-1 == 0) {
            action$choice <- arm
            return(action)
          }
        }
      }

      expected_rewards <- rep(0.0, context$k)

      ns     <- self$n_subjects
      P      <- unlist(self$theta$P)
      N      <- unlist(self$theta$N)
      C      <- unlist(self$theta$C)
      SS     <- unlist(self$theta$SS)
      p      <- unlist(self$theta$p[[user]])
      n      <- unlist(self$theta$n[[user]])

      sigmasq <- (ns * SS) / ((ns - 1) * N)
      M <- pmax((P * (1 - P) - sigmasq) / (sigmasq - ((P * (1 - P)) / ns) * C), 0)
      betas <- M / (M + n)
      betas[(M==0 | sigmasq==0)] <- 1

      p_mean   <- P + sqrt(2*log(self$theta$N_total)/N)
      p_choice <- p + sqrt(2*log(self$theta$n_total[[user]])/n)

      p_hat  <- betas * P + (1 - betas) * p

      action$choice <- which_max_tied(p_hat)

      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward

      inc(self$theta$N_total)           <- 1
      inc(self$theta$n_total[[user]])   <- 1

      inc(self$theta$n[[user]][[arm]])  <- 1
      inc(self$theta$p[[user]][[arm]])  <- (reward - self$theta$p[[user]][[arm]]) / self$theta$n[[user]][[arm]]

      inc(self$theta$N[[arm]])          <- 1
      inc(self$theta$P[[arm]])          <- (reward - self$theta$P[[arm]]) / self$theta$N[[arm]]
      self$theta$SS[[arm]]              <- self$theta$SS[[arm]]  - self$theta$ss[[user]][[arm]] + self$theta$n[[user]][[arm]] *
        (self$theta$p[[user]][[arm]] - self$theta$P[[arm]]) ^ 2
      self$theta$C[[arm]]               <- self$theta$C[[arm]] - self$theta$c[[user]][[arm]] +
        1/self$theta$n[[user]][[arm]]
      self$theta$c[[user]][[arm]]       <- 1 / self$theta$n[[user]][[arm]]
      self$theta$ss[[user]][[arm]]      <- self$theta$n[[user]][[arm]] * (self$theta$p[[user]][[arm]] - self$theta$P[[arm]]) ^ 2
      self$theta
    }
  )
)
