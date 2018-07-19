#' @export
YahooLinUCBHybridPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    alpha = NULL,
    class_name = "YahooLinUCBHybridPolicy",
    initialize = function(alpha = 1.0) {
      super$initialize()
      self$alpha  <- alpha
    },
    set_parameters = function() {
      dsda               <- length(self$d_per_arm)*length(self$d_shared)
      ds                 <- length(self$d_shared)

      self$theta         <- list( 'A0' = diag(1,dsda,dsda), 'A0_inv' = diag(1,dsda,dsda),
                                  'b0' = rep(0,dsda),'z' = matrix(0,ds,ds), 'x' = rep(0,ds))
      self$theta_to_arms <- list( 'A' = diag(1,ds,ds), 'A_inv' = diag(1,ds,ds),
                                  'B' = matrix(0,ds,dsda), 'b' = rep(0,ds))
    },
    get_action = function(t, context) {

      expected_rewards   <- rep(0.0, length(context$arms))
      local_arms         <- context$arms
      A0_inv             <- self$theta$A0_inv
      b0                 <- self$theta$b0
      beta_hat           <- A0_inv %*% b0

      for (arm in seq_along(local_arms)) {

        ################## unpack thetas ##############################################

        A                <-  self$theta$A[[local_arms[arm]]]
        A_inv            <-  self$theta$A_inv[[local_arms[arm]]]
        B                <-  self$theta$B[[local_arms[arm]]]
        b                <-  self$theta$b[[local_arms[arm]]]

        x                <- matrix(context$X[context$d_shared,1])
        z                <-  matrix(tcrossprod(context[['X']][self$d_per_arm,arm], x))

        ################## compute expected reward per arm #############################

        theta_hat        <-  A_inv %*% (b - B %*% beta_hat)

        tBAinvx          <- crossprod(B, (A_inv %*% x))
        txAinv           <- crossprod(x, A_inv)
        tzA0inv          <-crossprod(z, A0_inv)

        sd_one           <- tzA0inv %*% z
        sd_two           <- 2*(tzA0inv %*% tBAinvx)
        sd_three         <- txAinv %*% x
        sd_four          <- txAinv %*% (B %*% (A0_inv %*% tBAinvx))

        sd               <- sqrt(sd_one - sd_two  + sd_three + sd_four)

        mean             <- crossprod(z, beta_hat) + crossprod(x, theta_hat)

        expected_rewards[arm] <- mean + self$alpha * sd
      }

      ################## choose arm with highest expected reward #######################

      action$choice  <- context$arms[max_in(expected_rewards)]
      action
    },
    set_reward = function(t, context, action, reward) {

      #################### unpack thetas ###############################################

      arm          <- action$choice
      arm_index    <- which(context$arms == arm)
      reward       <- reward$reward

      z            <- matrix(tcrossprod(context$X[self$d_per_arm,arm_index],
                                        context$X[context$d_shared,arm_index]))

      x            <- matrix(context$X[context$d_shared,arm_index])


      A0           <- self$theta$A0
      A0_inv       <- self$theta$A0_inv
      b0           <- self$theta$b0
      A            <- self$theta$A[[arm]]
      A_inv        <- self$theta$A_inv[[arm]]
      B            <- self$theta$B[[arm]]
      b            <- self$theta$b[[arm]]

      #################### update thetas with returned reward & arm choice #############

      A0           <- A0 + (crossprod(B, A_inv) %*% B)
      b0           <- b0 + (crossprod(B, A_inv) %*% b)

      A            <- A + x %*% t(x)
      B            <- B + x %*% t(z)
      b            <- b + reward * x

      A_inv        <- sherman_morrisson(A_inv,as.vector(x))

      A0           <- A0 + tcrossprod(z,z) - (crossprod(B, A_inv) %*% B)
      b0           <- b0 + (reward * z) - (crossprod(B, A_inv) %*% b)

      A0_inv       <- inv(A0)

      #################### pack thetas ################################################

      self$theta$A0_inv       <- A0_inv
      self$theta$A0           <- A0
      self$theta$b0           <- b0
      self$theta$A[[arm]]     <- A
      self$theta$A_inv[[arm]] <- A_inv
      self$theta$B[[arm]]     <- B
      self$theta$b[[arm]]     <- b

      self$theta
    }
  )
)
