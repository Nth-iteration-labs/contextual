#' @export
ContextualEpochGreedyHybridPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    p = NULL,
    e = NULL,
    class_name = "ContextualEpochGreedyHybridPolicy",
    initialize = function(p = 10) {
      super$initialize()
      self$p <- p
      self$e <- 0
    },
    set_parameters = function() {
      dd <- self$d*self$d
      self$theta <- list('A0' = diag(1,dd,dd), 'A0_inv' = diag(1,dd,dd), 'b0' = rep(0,dd), 'n0' = 0)
      self$theta_to_arms <- list( 'A' = diag(1,self$d,self$d), 'B' = matrix(0,self$d,dd),
                                  'b' = rep(0,self$d))
    },
    get_action = function(t, context) {
      if (t <= self$p) {
        arm <- 1 + (t %% context$k)
        self$action$choice = arm
        return(self$action)
      }
      self$e <- rbinom(1,1,self$p/t)
      if(self$e==1) {
          arm <- sample.int(context$k, 1, replace = TRUE)
          self$action$choice = arm
          return(self$action)
      } else {
        expected_rewards <- rep(0.0, self$k)
        self$theta$A0_inv <- inv(self$theta$A0)
        beta_hat <- self$theta$A0_inv %*% self$theta$b0
        for (arm in 1:self$k) {

          ################## unpack thetas ##############################################

          A0         <-  self$theta$A0
          A          <-  self$theta$A[[arm]]
          B          <-  self$theta$B[[arm]]
          b          <-  self$theta$b[[arm]]
          z          <-  matrix(as.vector(outer(context$U,context$X[,arm])))
          x          <-  context$U
          A0_inv     <-  self$theta$A0_inv
          A_inv      <-  inv(A)

          ################## compute expected reward per arm #############################

          theta_hat  <-  A_inv %*% (b - B %*% beta_hat)
          mean <- (t(z) %*% beta_hat)  +  (t(x) %*% theta_hat)

          expected_rewards[arm] <- mean
        }
        self$action$choice  <- max_in(expected_rewards)
        return(self$action)
      }
    },
    set_reward = function(t, context, action, reward) {
      if (t <= self$p || self$e==1) {
        #################### unpack thetas ###############################################

        arm            <- action$choice
        reward         <- reward$reward
        z              <- matrix(as.vector(outer(context$U,context$X[,arm])))
        x              <- matrix(context$U)

        A0             <- self$theta$A0
        b0             <- self$theta$b0
        A              <- self$theta$A[[arm]]
        B              <- self$theta$B[[arm]]
        b              <- self$theta$b[[arm]]

        #################### update thetas with returned reward & arm choice #############

        A_inv          <- inv(A)

        A0             <- A0 + (t(B) %*% A_inv %*% B)
        b0             <- b0 + (t(B) %*% A_inv %*% b)

        A <- A + x %*% t(x)
        B <- B + x %*% t(z)
        b <- b + reward * x

        A_inv          <- inv(A)

        A0             <- A0 + (z %*% t(z)) - (t(B) %*% A_inv %*% B)
        b0             <- b0 + (reward * z) - (t(B) %*% A_inv %*% b)

        #################### pack thetas ################################################

        self$theta$A0       <- A0
        self$theta$b0       <- b0
        self$theta$A[[arm]] <- A
        self$theta$B[[arm]] <- B
        self$theta$b[[arm]] <- b
      }
      self$theta
    }
  )
)
#' Policy: A Time and Space Efficient Algorithm for Contextual Linear Bandits
#'
#' @name ContextualEpochGreedyHybridPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{
#' policy <- EpsilonGreedyPolicy(epsilon = 0.1)
#' }
#'
#'
NULL
