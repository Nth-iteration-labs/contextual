library(R6)
#' @export
SyntheticBandit <- R6Class(
  "SyntheticBandit",
  portable = FALSE, class = FALSE, cloneable = TRUE,
  private = list(W = NULL, R = NULL, X = NULL, oracle = NULL),
  public = list(
    d            = 0L,
    k            = 0L,
    means        = 0.0,
    stds         = 0.0,
    reward.type  = NULL,
    feature.type = NULL,
    weight.type  = NULL,
    initialize   = function(k = 2L,
                            d = 2L,
                            weight.type  = 'uniform',
                            reward.type  = 'binary',
                            feature.type = 'binary'  ) {
      self$k = k                                                                  # number of bandits
      self$d = d                                                                  # number of features
      self$reward.type = reward.type                                              # binary, positive, mixed
      self$feature.type = feature.type                                            # positive
      self$weight.type  = weight.type
      self$generate.weights()                                                     # generate some weights
    },
    generate.weights = function(mean = 0.0, sd = 1.0) {
      if (weight.type == "uniform") {
        private$W = matrix(runif(self$d * self$k), self$d, self$k)
      }
    },
    get.weights = function() {
      return(private$W)
    },
    set.weights = function(weightMatrix) {
      if (length(weightMatrix) != (self$d * self$k))
        stop("Weight needs to be of length k*d.")
      private$W = matrix(weightMatrix,  self$d, self$k)
    },
    generate.sample = function(n = 1L) {

      # n is not yet finished!

      if (self$feature.type == 'single' || is.na(self$feature.type)) {
        private$X = matrix(1, n, self$d)
      } else if (self$feature.type == 'binary') {
        private$X = matrix(sample( c(0,1), replace = TRUE, size = n * self$d ), n , self$d )  # always one feature, at least?
      }

      weights.per.feature = private$W * as.vector(private$X)

      if (self$reward.type == 'binary') {
        private$oracle = colSums(weights.per.feature)/sum(private$X)
        private$oracle[is.nan(private$oracle)] = 0
        private$R = as.integer(runif(self$k) < private$oracle  )
      }
      return(setNames(list(private$X, private$oracle), c("X","oracle")))
    },
    get.reward = function(action) {
      return(setNames(list( private$R[action], action, index.of.max(private$R) == action ),
                             c("reward","current.choice","is.optimal.choice")))
    },
    get.context = function() {
      return(generate.sample())
    }
  )
)
