ThompsonBayesianLinearPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "ThompsonBayesianLinearPolicy",
    J = NULL,
    P = NULL,
    err = NULL,
    initialize = function(J = matrix(c(0, 0.025, -0.025), nrow=1, ncol=3, byrow = TRUE),
                          P = matrix(diag(c(2,2,5)), nrow=3, ncol=3, byrow = TRUE),
                          err=1) {
      super$initialize()
      self$J <- J
      self$P <- P
      self$err <- err
    },
    set_parameters = function(context_params) {
      self$theta <- list('J' = self$J, 'P' = self$P, 'err' = self$err)
    },
    get_action = function(t, context) {
      sigma <- solve(self$theta$P, tol = 1e-200)
      mu <- sigma %*% matrix(self$theta$J)
      betas <- contextual::mvrnorm(n = 1, mu, sigma)
      action$choice <- -(betas[2] / (2*betas[3]))
      if(action$choice > 1){
        action$choice <- 1
      } else if(action$choice < 0) {
        action$choice <- 0
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      y <- reward$reward
      x <- action$choice
      x <- matrix(c(1,x,x^2), nrow = 1, ncol = 3, byrow = TRUE)
      self$theta$J <- (x*y)/self$theta$err + self$theta$J
      self$theta$P <- t(x)%*%x + self$theta$P
      self$theta
    }
  )
)
