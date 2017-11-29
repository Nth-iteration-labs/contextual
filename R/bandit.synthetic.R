library(R6)
#' @export
SyntheticBandit <- R6Class(
  "SyntheticBandit",
  portable = FALSE, class = FALSE, cloneable = TRUE,
  private = list(W = NULL, R = NULL, X = NULL),
  public = list(
    d            = 0,
    k            = 0,
    means        = 0.0,
    stds         = 0.0,
    reward_type  = NULL,
    feature_type = NULL,
    weight_type  = NULL,
    initialize   = function(k = 2L,
                            d = 2L,
                            weight_type  = 'uniform',
                            reward_type  = 'binary',
                            feature_type = 'binary'  ) {

      self$k = k                                                                  # number of bandits
      self$d = d                                                                  # number of features
      self$reward_type = reward_type                                              # binary, positive, mixed
      self$feature_type = feature_type                                            # positive
      self$weight_type  = weight_type
      self$generate_weights()                                                     # generate some weights
    },
    generate_weights = function(mean = 0.0, sd = 1.0) {
      if (weight_type == "uniform") {
        private$W = matrix(runif(self$k * self$d), self$k, self$d)
      }
    },
    get_weights = function() {
      return(private$W)
    },
    set_weights = function(weightMatrix) {
      private$W = matrix(weightMatrix, self$k , self$d)
    },
    generate_sample = function(n = 1L) {

      # n is not yet finished!

      if (self$feature_type == 'single' || is.na(self$feature_type)) {
        private$X = matrix(1, n, self$d)
      } else if (self$feature_type == 'binary') {
        private$X = matrix(sample( c(0,1), replace = TRUE, size = n * self$d ), n , self$d )  # always one feature, at least?
      }

      weights_per_feature = t(t(private$W) * as.vector(private$X))                 # this can be done in a better way!

      if (self$reward_type == 'binary') {
        randomcomparator   = runif(self$k)
        featureweights = rowSums(weights_per_feature)/sum(private$X)
        featureweights[is.nan(featureweights)] = 0
        private$R = as.integer(randomcomparator < featureweights  )
      }
      return(setNames(list(private$X, private$R, private$W), c("X","R","W")))
    },
    get_reward = function(action) {
      return(setNames(list( private$R[action],index_of_max(private$R) == action, action),c("reward","optimal","arm")))
    },
    get_context = function() {
      generate_sample()
      return(private$X)
    },
    add_arm = function() {
    },
    remove_arm = function() {

    }
  )
)
