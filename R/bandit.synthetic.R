library(R6)
#' @export
SyntheticBandit <- R6Class(
  "SyntheticBandit",
  portable = FALSE, class = FALSE, cloneable = TRUE,
  private = list(W = NULL, R = NULL, X = NULL),
  public = list(
    d           = 0,
    k           = 0,
    means       = 0.0,
    stds        = 0.0,
    reward_type = NULL,
    feature_type = NULL,
    initialize = function(k = 2L,
                          d = 2L,
                          reward_type = 'binary',
                          feature_type = 'binary'  ) {
      self$d = d                                                                  # number of features
      self$k = k                                                                  # number of bandits
      self$reward_type = reward_type                                              # binary, positive, mixed
      self$feature_type = feature_type                                            # positive
      self$reset()
    },
    reset = function(mean = 0.0, sd = 1.0) {
      self$means = rnorm(self$k)                                                  # means  vector of k size
      self$stds = 1.0 + 2.0 * runif(self$k)                                       # stddev vector of k size - wild!
      #private$W  = matrix(rnorm(self$k * self$d, mean, sd) , self$k , self$d)     # generate weight vectors
      private$W = matrix(runif(self$k))
      private$W = matrix(c(0.1,0.1,0.9), self$k , self$d)
    },
    get_weights = function() {
      return(private$W)
    },
    generate_samples = function(n = 1000L) {
      if (self$feature_type == 'binary') {
        private$X = matrix(sample( c(0,1), replace = TRUE, size = n * self$d ), n , self$d )
      } else if (self$feature_type == 'integer') {
        private$X = matrix(sample( c(0:4), replace = TRUE, size = n * self$d ), n , self$d )
      }

      IP = tcrossprod(private$X,private$W)

      if (self$reward_type == 'gaussian') {
        ############### work this out! + IP
        private$R = matrix(rnorm(self$means + IP, self$means  + IP, self$stds), ncol = self$k)
      } else if (self$reward_type == 'binary') {
        randomcompare = matrix(runif(self$k))
        private$R = as.integer(randomcompare < private$W  )
        #private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)
      } else if (self$reward_type == 'positive') {
        private$R = matrix(rnorm(self$means + IP, self$means + IP, self$stds), ncol = self$k)
      } else if (self$reward_type == 'lognormal') {
        private$R = matrix(rlnorm(self$means + IP, self$means + IP, self$stds), ncol = self$k)
      } else if (self$reward_type == 'mixed') {
        private$R = matrix((sign(rnorm(self$means + IP, self$means + IP, self$stds)) + 1) / 2, ncol = self$k)
        private$R = private$R * matrix(rnorm(self$means + IP, self$means + IP, self$stds) , ncol = self$k)
      }
      return(setNames(list(private$X, private$R, private$W), c("X","R","W")))
    },
    get_reward = function(action) {
      return(setNames(list( private$R[action],index_of_max(private$R) == action, action),c("reward","optimal","arm")))
    },
    get_context = function() {
      generate_samples(1L)
      return(private$X)
    },
    add_arm = function () {
    },
    remove_arm = function() {

    }
  )
)
