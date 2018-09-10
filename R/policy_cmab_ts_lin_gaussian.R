#' @export
ContextualThompsonHybrid <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    lambda_prior = NULL,
    a_init = NULL,
    b_init = NULL,
    class_name = "ContextualThompsonHybrid",
    initialize = function(a_init, b_init, lambda_prior) {
      super$initialize()
      self$lambda_prior <- lambda_prior
      self$a_init       <- a_init
      self$b_init       <- b_init
    },
    set_parameters = function(context_params) {
      d                  <- context_params$d
      self$theta         <- list( 'a0' = self$a_init, 'b0' = self$b_init)
      self$theta_to_arms <- list( 'a' = self$a_init, 'b' = self$b_init,
                                  'cov' = (1.0 / self$lambda_prior) * diag(1,d,d),
                                  'precision' = self$lambda_prior * diag(1,d,d),
                                  'mu' = rep(0,d), 'n' = 0)
    },
    get_action = function(t, context) {

      n_zero_arms <- which(self$theta$n == 0)

      if (length(n_zero_arms) > 0) {
        action$choice <- sample_one_of(n_zero_arms)
        return(action)
      }
      expected_rewards <- rep(0.0, context$k)
      for (arm in 1:context$k) {
        sigma2_s              <- as.numeric(self$theta$b[[arm]] * rinvgamma(1,self$theta$a[[arm]]))
        beta_s                <- mvrnorm(1, self$theta$mu[[arm]], sigma2_s * self$theta$cov[[arm]])

        print(t(beta_s) %*% context$X[,arm]+matrix(t(beta_s),context$d,context$d))
        expected_rewards[arm] <- t(beta_s) %*% context$X[,arm] + as.matrix(t(beta_s),context$d,context$d)
      }

      action$choice  <- max_in(expected_rewards)
      action
    },
    set_reward = function(t, context, action, reward) {

      arm                         <- action$choice
      reward                      <- reward$reward
      Xa                          <- context$X[,arm]
      inc(self$theta$n[[arm]])    <- 1

      s                           <- Xa %*% t(Xa)

      precision_a                 <- s + self$lambda_prior * self$lambda_prior * diag(1,context$d,context$d)
      cov_a                       <- inv(precision_a)


      mu_a                        <- cov_a %*% (Xa * reward)


      a_post                      <- self$theta$a0 + context$k / 2.0

      b_upd                       <- 0.5 * reward * reward - t(mu_a) %*% (precision_a %*% mu_a)
      b_post                      <- self$theta$b0 + b_upd

      self$theta$mu[[arm]]        <- mu_a
      self$theta$cov[[arm]]       <- cov_a
      self$theta$precision[[arm]] <- precision_a
      self$theta$a[[arm]]         <- a_post
      self$theta$b[[arm]]         <- b_post

      self$theta
    }
  )
)
#' Policy: A Time and Space Efficient Algorithm for Contextual Linear Bandits
#'
#' @name ContextualThompsonHybrid
#' @family contextual subclasses
NULL
