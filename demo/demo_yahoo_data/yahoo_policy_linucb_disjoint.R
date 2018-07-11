YahooLinUCBDisjointPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "YahooLinUCBDisjointPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms <- list( 'A' = diag(1,self$d,self$d), 'b' = rep(0,self$d),
                                  'A_inv' = solve(diag(1,self$d,self$d)))

    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, length(context$arms))

      for (arm in context$arms) {

        X          <-  context$X[,arm]

        A          <-  self$theta$A[[arm]]
        A_inv      <-  self$theta$A_inv[[arm]]
        b          <-  self$theta$b[[arm]]


        theta_hat  <-  A_inv %*% b

        mean       <-  X %*% theta_hat
        sd         <-  sqrt(tcrossprod(X %*% A_inv, X))

        expected_rewards[arm] <- mean + self$alpha * sd
      }
      action$choice  <- max_in(expected_rewards)

      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      Xa     <- context$X[,arm]
      A_inv  <-  self$theta$A_inv[[arm]]

      # Sherman-Morrison inverse
      outer_Xa <- outer(Xa, Xa)
      self$theta$A_inv[[arm]]  <- A_inv - c((A_inv %*% (outer_Xa %*% A_inv))) / c(1.0+ (crossprod(Xa,A_inv) %*% Xa))

      self$theta$A[[arm]]      <- self$theta$A[[arm]] + outer_Xa
      self$theta$b[[arm]]      <- self$theta$b[[arm]] + reward * Xa

      self$theta
    }
  )
)
