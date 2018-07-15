YahooLinUCBDisjointPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    sparse = NULL,
    class_name = "YahooLinUCBDisjointPolicy",
    initialize = function(alpha = 0.2, sparse = 0.0) {
      super$initialize()
      self$sparse <- sparse
      self$alpha  <- alpha
    },
    set_parameters = function() {
      self$theta_to_arms <- list( 'A' = diag(1,self$d,self$d), 'b' = rep(0,self$d),
                                  'A_inv' = solve(diag(1,self$d,self$d)))
    },
    get_action = function(t, context) {

      expected_rewards <- rep(0.0, length(context$arms))

      local_arms       <- context$arms

      # Carefull here: there are arm numbers in the order in context matrix: 20,19,17,1,4,..
      # and the index of the arms within the current context: 1,2,3,4,..

      for (arm in seq_along(local_arms)) {

        # if first part of vector is disjoint user data, it's the same for each arm

        X          <-  context$X[7:12,arm]

        A          <-  self$theta$A[[local_arms[arm]]]
        A_inv      <-  self$theta$A_inv[[local_arms[arm]]]
        b          <-  self$theta$b[[local_arms[arm]]]


        theta_hat  <-  A_inv %*% b

        mean       <-  X %*% theta_hat
        sd         <-  sqrt(tcrossprod(X %*% A_inv, X))


        expected_rewards[arm] <- mean + self$alpha * sd
      }

      action$choice  <- context$arms[max_in(expected_rewards)]

      action
    },
    set_reward = function(t, context, action, reward) {

      if (runif(1) < self$sparse) return(self$theta)

      arm                      <- action$choice
      arm_index                <- which(context$arms == arm)
      reward                   <- reward$reward

      # TODO: make all Xa and arm X into X_a everywhere

      X_a                       <- context$X[7:12,arm_index]  # same for each arm here if disjoint user

      A_inv                    <- self$theta$A_inv[[arm]]

      self$theta$A_inv[[arm]]  <- sherman_morrisson(self$theta$A_inv[[arm]],X_a)

      self$theta$A[[arm]]      <- self$theta$A[[arm]] + outer(X_a, X_a)
      self$theta$b[[arm]]      <- self$theta$b[[arm]] + reward * X_a

      self$theta
    }
  )
)
