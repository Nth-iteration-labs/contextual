library(R6)

#' @export
Bandit <- R6Class(
  "Bandit",

  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,

  private = list(W = NULL, R = NULL, X = NULL),

  public = list(
    d = 1L,
    k = 3L,
    reward_type = '',
    means = 0.0,
    stds =  0.0,

    initialize = function(k = 3L,
                          d = 1L,
                          reward_type = 'gaussian') {
      self$d = d                                                                  # number of features
      self$k = k                                                                  # number of bandits
      self$reward_type = reward_type                                              # binary, positive, mixed
      self$reset()
    },

    reset = function(mean = 0.0, sd = 1.0) {
      self$means = rnorm(self$k)                                                  # means  vector of k size
      #self$stds = 1.0 + 2.0 * runif(self$k)                                      # stddev vector of k size - wild!
      self$stds  = 0.0
      private$W  = matrix(rnorm(self$k * self$d, mean, sd) , self$k , self$d)     # generate weight vectors
      #private$W = matrix(c(0.1,0.1,0.9), self$k , self$d)
    },

    generate_samples = function(n = 1000L) {

      private$X = sample( c(0,1), replace = TRUE, size = n * self$d )

      #IP = private$X %*% t(private$W)                             # the rewards are functions of the inner products with W

      IP = tcrossprod(private$X,private$W)

      if (self$reward_type == 'gaussian') {
        private$R = matrix(rnorm(self$means + IP, self$means, self$stds), ncol = self$k)   ############### work this out! + IP ...

      } else if (self$reward_type == 'binary') {
        private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)

      } else if (self$reward_type == 'positive') {
        private$R = matrix(rlnorm(self$means + IP, self$means + IP, self$stds), ncol = self$k)

      } else if (self$reward_type == 'mixed') {
        private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)
        private$R = private$R * matrix(rnorm(self$means + IP, self$means + IP, self$stds) , ncol = self$k)

      }
      return(setNames(list(private$X, private$R, private$W), c("X","R","W")))# still pregenerate, for order troubles?
    },

    get_rewards = function() {  # at time t?
      return(private$R)
    },

    get_reward = function(action) {
      return(
        setNames(
          list(  private$R[action],   index_of_max(private$R) == action,   action  ),    #  Private R -- not W!!!!!! remember that!
             c(  "reward",            "optimal",                           "arm"   )
        )
      )
    },

    get_context = function() {
      generate_samples(1L)
      return(private$X)
    }

  )
)
