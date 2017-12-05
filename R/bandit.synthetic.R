library(R6)
#' @export
SyntheticBandit <- R6Class(
  "SyntheticBandit",
  inherit = Contextual,
  portable = FALSE, class = FALSE, cloneable = TRUE,
  private = list(W = NULL, R = NULL, X = NULL, oracle = NULL),
  public = list(
    d            = 0L,
    k            = 0L,
    means        = 0.0,
    stds         = 0.0,
    reward.family  = NULL,
    feature.type = NULL,
    weight.distribution  = NULL,
    initialize   = function(k = 2L,
                            d = 2L,
                            weight.distribution  = 'Uniform',
                            reward.family        = 'Bernoulli',
                            feature.type         = 'Bernoulli'  ) {

      self$k = k
      self$d = d
      self$reward.family        = reward.family
      self$feature.type         = feature.type
      self$weight.distribution  = weight.distribution
      self$generate.weights()
    },
    generate.weights = function(mean = 0.0, sd = 1.0) {
      if (weight.distribution == "Uniform") {
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

      # n is not yet completed!

      if (self$feature.type == 'Single' || is.na(self$feature.type)) {
        private$X = matrix(1, n, self$d)
      } else if (self$feature.type == 'Bernoulli') {
        private$X = matrix(0, n , self$d )                                                       # create matrix
        private$X[sample(1:(n * self$d), 1)] = 1                                                 # always one feature, at least?
        private$X = as.integer( private$X |
                                  matrix(sample( c(0,1),
                                    replace = TRUE,
                                    size = n * self$d ), n , self$d ) )                          # but can be multiple features
      }

      weights.per.feature = private$W * as.vector(private$X)

      if (self$reward.family == 'Bernoulli') {
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

#' External SyntheticBandit
#'
#' SyntheticBandit intro
#'
#' @section Usage:
#' \preformatted{b <- SyntheticBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{SyntheticBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new SyntheticBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name SyntheticBandit
#' @examples
#'\dontrun{}
#'
NULL

