#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = TRUE,
  private = list(
    W = NULL,
    R = NULL,
    X = NULL,
    oracle = NULL
  ),
  public = list(
    d            = 0L,
    k            = 0L,
    means        = 0.0,
    stds         = 0.0,
    reward_family  = NULL,
    feature_type = NULL,
    weight_distribution  = NULL,
    initialize   = function(k = 2L,
                            d = 2L,
                            weight_distribution  = 'Uniform',
                            reward_family        = 'Bernoulli',
                            feature_type         = 'Bernoulli') {
      self$k = k
      self$d = d
      self$reward_family        = reward_family
      self$feature_type         = feature_type
      self$weight_distribution  = weight_distribution
      self$generate_weights()
    },
    generate_weights = function(mean = 0.0, sd = 1.0) {
      if (weight_distribution == "Uniform") {
        private$W = matrix(runif(self$d * self$k), self$d, self$k)
      }
    },
    get_weights = function() {
      private$W
    },
    set_weights = function(weight_matrix) {
      if (length(weight_matrix) != (self$d * self$k))
        stop("Weight needs to be of length k*d.")
      private$W = matrix(weight_matrix,  self$d, self$k)
    },
    generate_sample = function(n = 1L) {
      # n is not yet completed!

      if (self$feature_type == 'Single' ||
          is.na(self$feature_type)) {
        private$X = matrix(1, n, self$d)
      } else if (self$feature_type == 'Bernoulli') {
        private$X = matrix(0, n , self$d)                                       # create matrix
        private$X[sample(1:(n * self$d), 1)] = 1                                # always one feature, at least?
        private$X = as.integer(private$X |
                                 matrix(sample(
                                   c(0, 1),
                                   replace = TRUE,
                                   size = n * self$d
                                 ), n , self$d))                                # but can be multiple features
      }

      weights_per_feature = private$W * as.vector(private$X)

      if (self$reward_family == 'Bernoulli') {
        private$oracle = colSums(weights_per_feature) / sum(private$X)
        private$oracle[is.nan(private$oracle)] = 0
        private$R = as.integer(runif(self$k) < private$oracle)
      }
      setNames(list(private$X, private$oracle), c("X", "oracle"))
    },
    get_reward = function(action) {
      setNames(
        list(
             private$R[action$current_choice],
             action$current_choice,
             index_of_max(private$R) == action$current_choice,
             action$propensity),

           c("reward",
             "current_choice",
             "is_optimal_choice",
             "propensity")
      )
    },
    get_context = function() {
      generate_sample()
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
