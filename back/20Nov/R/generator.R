library(R6)

#' @export
Bandit <- R6Class(
  "Bandit",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  private = list(W = NULL, R = NULL, X = NULL),

  public = list(
    d = NULL,
    k = NULL,
    reward_type = NULL,
    means = NULL,
    stds = NULL,

    initialize = function(k = 3,
                          d = 10,
                          reward_type = 'binary') {
      self$d = d                                                                 # dimension of the feature vector
      self$k = k                                                                 # number of bandits
      self$reward_type = reward_type                                             # binary, positive, mixed

      self$reset()
    },

    reset = function(mean = 0.0, sd = 1.0) {
      self$means = rnorm(self$k)                                                  # means  vector of k size
      self$stds = 1 + 2 * runif(self$k)                                           # stddev vector of k size
      private$W = matrix(rnorm(self$k * self$d, mean, sd) , self$k , self$d)      # generate weight vectors
    },

    generate_samples = function(n = 1000) {
      # the X are only binary
      private$X = matrix(rbinom(n * self$d, 1, 0.5), n, self$d)

      # the rewards are functions of the inner products with W
      IP = private$X %*% t(private$W)

      if (self$reward_type == 'binary') {
        private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)
      } else if (self$reward_type == 'positive') {
        private$R = matrix(rlnorm(self$means + IP, self$means + IP, self$stds), ncol = self$k)
      } else if (self$reward_type == 'mixed') {
        private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)
        private$R = private$R * matrix(rnorm(self$means + IP, self$means + IP, self$stds) , ncol = self$k)
      }
      samples = setNames(list(private$X, private$R, private$W), c("X","R","W"))   # still pregenerate, for order troubles?
      return(samples)
    },

    get_rewards = function() {  # at time t?
      return(private$R)
    },

    get_reward = function(action) {
      return(private$R[action])
    }

    get_context = function() {
      generate_samples(1)
      return(private$X)
    }

  )
)

bandit = Bandit$new(3,10, 'binary')

samples = bandit$generate_samples(10)

print(bandit$get_context())
print(bandit$get_rewards())
print(bandit$get_rewards())

print(bandit$get_context())
print(bandit$get_rewards())
print(bandit$get_rewards())


