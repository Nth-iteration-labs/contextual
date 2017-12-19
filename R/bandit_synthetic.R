# create from scratch if not cache, otherwise do cahce
#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = AbstractBandit,
  portable = FALSE,
  class = FALSE,
  public = list(
    d            = 0L,
    k            = 0L,
    means        = 0.0,
    stds         = 0.0,
    reward_family  = NULL,
    feature_type = NULL,
    weight_distribution  = NULL,
    initialize   = function(weight_distribution  = 'Uniform',
                            reward_family        = 'Bernoulli',
                            feature_type         = 'Bernoulli') {
      super$initialize()
      self$set_precaching(TRUE)
      self$reward_family        <- reward_family
      self$feature_type         <- feature_type
      self$weight_distribution  <- weight_distribution
    },
    get_reward = function(action, t) {
      do_reward(action, t)
    },
    get_context = function(t) {
      do_context(t)
    },
    generate_weights = function(k, d, mean = 0.0, sd = 1.0) {
      self$k <- k
      self$d <- d
      if (self$weight_distribution == "Uniform") {
        private$.W <- matrix(runif(self$d * self$k), self$d, self$k)
      }
      invisible(self)
    },
    generate_samples = function(n = 1L, d_one_min = TRUE) {
      private$.X <- matrix(0, n , d)
      private$.R <- matrix(0, self$k, n)
      private$.O <- matrix(0, self$k, n)

      if (self$feature_type == 'Bernoulli') {
        if (d_one_min) private$.X[cbind(1:n, sample(d, n, replace = TRUE))] <- 1
        X_random <- matrix(sample(c(0, 1), replace = TRUE, size = n * self$d), n , self$d)
        private$.X <- matrix((private$.X | X_random),n,d)
        mode(private$.X) <- 'integer'
      }

      W <- array(t(matrix(private$.W, self$k , self$d)), dim = c(self$d, self$k, n))

      localX <- private$.X
      for (nn in 1:n) {
        W[, , nn] <- W[, , nn] * as.vector(localX[nn, ])
      }
      private$.O <- t(t(colSums(W)) / as.vector(rowSums(private$.X)))
      private$.O[is.nan(private$.O)] <- 0

      if (self$reward_family == 'Bernoulli') {
        private$.R <- runif(self$k * n) < private$.O
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
