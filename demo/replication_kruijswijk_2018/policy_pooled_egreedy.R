UnpooledEgreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UnpooledEgreedyPolicy",
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1) {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta <- list(n = rep(list(as.list(rep(0, context_params$k))), self$n_subjects),
                         mu = rep(list(as.list(rep(0, context_params$k))), self$n_subjects))
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
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PooledEgreedyPolicy",
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1) {
      super$initialize()
      self$epsilon <- epsilon
    },
    set_parameters = function(context_params) {
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(t, context) {
      if (runif(1) > epsilon) {
        action$choice <- which_max_list(self$theta$MU)
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

PartiallyPooledEgreedyPolicyOld <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledEgreedyPolicyOld",
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1) {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta         <- list(n = rep(list(as.list(rep(1, context_params$k))),self$n_subjects),
                                 mu = rep(list(as.list(rep(0, context_params$k))),self$n_subjects))
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        betas <- 1 / sqrt(unlist(self$theta$n[[user]]))
        gmus  <- unlist(self$theta$MU)
        umus  <- unlist(self$theta$mu[[user]])
        p_hat  <- betas * gmus + (1 - betas) * umus
        action$choice <- which_max_tied(p_hat)
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

PartiallyPooledEgreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledEgreedyPolicy",
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1) {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta         <- list(n = rep(list(as.list(rep(1, context_params$k))),self$n_subjects),
                                 mu = rep(list(as.list(rep(0, context_params$k))),self$n_subjects))
      self$theta_to_arms <- list('N' = 0, 'MU' = 0)
    },
    get_action = function(t, context) {
      user <- context$user_context
      if (runif(1) > epsilon) {
        #betas <- 1 / sqrt(unlist(self$theta$n[[user]]))
        betas <- 2 / (2 + unlist(self$theta$n[[user]]))
        gmus  <- unlist(self$theta$MU)
        umus  <- unlist(self$theta$mu[[user]])
        p_hat  <- betas * gmus + (1 - betas) * umus
        action$choice <- which_max_tied(p_hat)
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


PartiallyBBPooledEgreedyPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "PartiallyPooledBBEgreedyPolicy",
    epsilon = NULL,
    n_subjects = NULL,
    initialize = function(epsilon = 0.1, n_subjects = 1) {
      super$initialize()
      self$epsilon <- epsilon
      self$n_subjects <- n_subjects
    },
    set_parameters = function(context_params) {
      self$theta         <- list(n  = rep(list(as.list(rep(1, context_params$k))),self$n_subjects),
                                 p  = rep(list(as.list(rep(0, context_params$k))),self$n_subjects),
                                 ss = rep(list(as.list(rep(0, context_params$k))),self$n_subjects),
                                 c  = rep(list(as.list(rep(0, context_params$k))),self$n_subjects))
      self$theta_to_arms <- list("N"  = 1, "P" = 0, "SS" = 0, "C" = 0)
    },
    get_action = function(t, context) {

      user <- context$user_context

      if (runif(1) > epsilon) {

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

        p_hat  <- betas * P + (1 - betas) * p

        action$choice <- which_max_tied(p_hat)

      } else {
        action$choice <- sample.int(context$k, 1, replace = TRUE)
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      user   <- context$user_context
      reward <- reward$reward

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
