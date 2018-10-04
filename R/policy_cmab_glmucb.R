#' @importFrom nleqslv nleqslv
#' @export
GlmUCBPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    rewards = NULL,
    contexts = NULL,
    d = NULL,
    link = NULL,
    class_name = "GlmUCBPolicy",
    initialize = function(link_function = function(x) exp(x) / (1 + exp(x)) ) {
      super$initialize()
      self$rewards  <- list()
      self$contexts <- list()
      self$link     <- link_function
    },
    set_parameters = function(context_params) {
      self$d <- context_params$d
      self$theta <- list( 'M' = diag(1,d,d), 'prev' = rep(0,d),
                          'M_inv' = solve(diag(1,d,d)))
      self$theta_to_arms <- list('n' = 0)
    },
    get_action = function(t, context) {


      n_zero_arms <- which(self$theta$n == 0)
      if (length(n_zero_arms) > 0) {
        action$choice <- sample_one_of(n_zero_arms)
        return(action)
      }

      theta_hat       <- nleqslv::nleqslv(self$theta$prev,self$to_optimize)$x
      self$theta$prev <- theta_hat

      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        expected_rewards[arm] <- self$acquisition_function(theta_hat,context$X[,arm])
      }
      action$choice  <- which_max_tied(expected_rewards)

      action
    },
    set_reward = function(t, context, action, reward) {
      arm                      <- action$choice
      reward                   <- reward$reward
      Xa                       <- context$X[, arm]
      self$rewards             <- append(self$rewards,reward)
      self$contexts            <- append(self$contexts,list(Xa))

      inc(self$theta$n[[arm]]) <- 1
      inc(self$theta$M)        <- outer(Xa, Xa)
      self$theta$M_inv         <- inv(self$theta$M)
      self$theta
    },
    acquisition_function = function (th, Xa){
      t <- length(self$rewards)
      exploit <- self$link(Xa %*% th)
      explore <- sqrt(3 * log(t)) * sqrt(((t(Xa) %*% self$theta$M_inv)%*% Xa))
      exploit + explore
    },
    to_optimize = function(x) {
      sum_matrix <- matrix(0.0, length(self$rewards), self$d)
      for (t in seq_along(self$rewards)) {
        R_k             <- self$rewards[[t]]
        mu_k            <- self$link(x %*% self$contexts[[t]])
        m_ak            <- self$contexts[[t]]
        sum_matrix[t,]  <- as.numeric(R_k - mu_k) * m_ak
      }
      colSums(sum_matrix)
    }
  )
)

#' Policy: GlmUCBPolicy
#'
#' @name GlmUCBPolicy
NULL
