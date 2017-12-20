# create from scratch if not cache, otherwise do cahce
#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = AbstractBandit,
  portable = FALSE,
  class = FALSE,
  public = list(
    d             = NULL,
    k             = NULL,
    weight_stds   = NULL,
    weight_means  = NULL,
    reward_means  = NULL,
    reward_stds   = NULL,
    reward_type   = NULL,
    weight_distribution  = NULL,
    initialize   = function(weight_distribution  = 'Uniform',
                            reward_type          = 'Bernoulli',

                            weight_stds          = NA,
                            weight_means         = NA,
                            reward_means         = NA,
                            reward_stds          = NA) {

      super$initialize()
      self$is_precaching        <- TRUE
      self$reward_type          <- reward_type
      self$weight_distribution  <- weight_distribution
    },
    get_reward = function(action, t) {
      format_reward(action, t)
    },
    get_context = function(t) {
      format_context(t)
    },
    generate_weights = function(k, d = 1, mean = 3.0, sd = 1.0) {
      self$k <- k
      self$d <- d
      if (self$weight_distribution == "Uniform") {
        private$.W <- matrix(runif(self$d * self$k), self$d, self$k)
      } else if (self$weight_distribution == "Normal") {
        private$.W <- matrix(rnorm(self$d * self$k, self$feature_means, self$feature_stds), self$d, self$k)
      }
      invisible(self)
    },
    generate_cache = function(n = 1L, never_zero_features = TRUE) {
      private$.X <- matrix(0, n , d)
      private$.O <- matrix(0, self$k, n)
      private$.R <- matrix(0, self$k, n)

      private$.generate_context(n, never_zero_features)
      private$.generate_oracle(n)
      private$.generate_rewards(n)
    }
  ),
  private = list(
    .generate_context = function(n = 1L, never_zero_features = TRUE) {
      if (never_zero_features) private$.X[cbind(1:n, sample(d, n, replace = TRUE))] <- 1
      X_random <- matrix(sample(c(0, 1), replace = TRUE, size = n * self$d), n , self$d)
      private$.X <- matrix((private$.X | X_random),n,d)
      mode(private$.X) <- 'integer'
    },
    .generate_oracle = function(n) {
      W <- array(t(matrix(private$.W, self$k , self$d)), dim = c(self$d, self$k, n))
      for (i in 1:n) { W[, , i] <- W[, , i] * as.vector(private$.X[i, ])}
      private$.O <- t(t(colSums(W)) / as.vector(rowSums(private$.X)))
      private$.O[is.nan(private$.O)] <- 0
    },
    .generate_rewards = function(n) {
      if (self$reward_type == 'Bernoulli') {
        private$.R <- runif(self$k * n) < private$.O
      } else if (self$reward_type == 'Gaussian') {
        private$.R <- (rnorm(self$k * n, self$reward_means, self$reward_stds) + private$.O) / 2
      }
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
