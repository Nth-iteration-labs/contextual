EFirstRegressionPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "EFirstRegressionPolicy",
    b = NULL,
    A = NULL,
    epsilon = NULL,
    initialize = function(b = matrix(c(0, 0.025, -0.025), nrow=1, ncol=3, byrow = TRUE),
                          A = matrix(diag(c(2,2,5)), nrow=3, ncol=3, byrow = TRUE),
                          epsilon) {
      super$initialize()
      self$b <- b
      self$A <- A
      self$epsilon <- epsilon
      print("initialize")
    },
    set_parameters = function(context_params) {
      self$theta <- list('b' = self$b, 'A' = self$A, 'epsilon' = self$epsilon)
      print("set_parameters")
    },
    get_action = function(t, context) {
      print("get_action")
      if(t <= epsilon){
        action$choice <- runif(1)
      } else {
        betas <- solve(self$theta$A) %*% t(self$theta$b) #np.linalg.inv(self.value['A']) * self.value['b'].T
        action$choice <- -(betas[2] / (2*betas[3]))
        action
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      print("set_reward")
        y <- reward$reward
        x <- action$choice
        x <- matrix(c(1,x,x^2), nrow = 1, ncol = 3, byrow = TRUE)
        self$theta$b <- (x*y) + self$theta$b
        self$theta$A <- t(x)%*%x + self$theta$A
        self$theta
    }
  )
)
