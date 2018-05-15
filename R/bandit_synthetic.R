#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  public = list(

    d             = NULL,
    k             = NULL,
    reward_means  = NULL,
    reward_stds   = NULL,
    reward_family = NULL,
    has_cache     = NULL,
    precaching    = NULL,
    not_zero_features = NULL,
    random_one_feature = NULL,
    weights   = NULL,
    initialize   = function(
      reward_family        = 'Bernoulli',
      reward_means         = 4.0,
      reward_stds          = 1.0,
      weights              = NULL,
      precaching           = TRUE,
      not_zero_features    = TRUE,
      random_one_feature   = FALSE
    ) {
      if (!(reward_family %in% c("Bernoulli","Gaussian","Poisson"))) {
        stop('Reward family needs to be one of "Bernoulli", "Gaussian" or "Poisson".' , call. = FALSE)
      }

      if (is.vector(self$weights)) self$weights <- matrix(self$weights, nrow = 1L)

      super$initialize(weights)
      self$has_cache            <- FALSE
      self$precaching           <- precaching
      self$reward_family        <- reward_family
      self$reward_means         <- reward_means
      self$reward_stds          <- reward_stds
      self$not_zero_features    <- not_zero_features
      self$random_one_feature   <- random_one_feature
    },
    set_weights = function(local_W) {
      if (is.vector(local_W)) private$W <- matrix(local_W, nrow = 1L)
      if (is.matrix(local_W)) private$W <- local_W
      self$d <- as.integer(dim(private$W)[1])
      self$k <- as.integer(dim(private$W)[2])
      invisible(private$W)
    },
    get_context = function(t) {
      if (self$precaching) {
        private$context_to_list(t)
      } else {
        self$generate_bandit_data(n = 1L)
        private$context_to_list(t = 1)
      }
    },
    get_reward = function(t, context, action) {
      private$reward_to_list(t, action)
    },

    generate_bandit_data = function(n = 1L,
                                    silent = TRUE ) {
      if (!silent) message("Precaching bandit" )
      private$X <- array(0, dim = c(self$d, self$k, n))
      private$O <- matrix(0, self$k, n)
      private$R <- matrix(0, self$k, n)
      private$generate_context(n)
      private$generate_weights(n)
      private$generate_rewards(n)
      self$has_cache <- TRUE
    }
  ),
  private = list(
    W = NULL,
    R = NULL,
    X = NULL,
    O = NULL,
    generate_context = function(n = 1L) {
      context_mask <- matrix(0, n , self$d)
      if (self$not_zero_features | self$random_one_feature) {
        context_mask[cbind(1L:n, sample(self$d, n, replace = TRUE))] <- 1
      }
      if (!self$random_one_feature) {
        context_mask <- matrix((context_mask | matrix(sample(c(0, 1), replace = TRUE, size = n * self$d),  n, self$d)), n, self$d)
      }
      mode(context_mask) <- 'integer'
      private$X <- array(0, dim = c(self$d, self$k, n))
      for (i in 1:n) {
        private$X[,,i] <- matrix( context_mask[i,], self$d, self$k)
      }
      private$X
    },
    generate_weights = function(n) {
      weight_array  <- array(t(matrix( private$W, self$k , self$d, byrow = TRUE )), dim = c(self$d, self$k, n))
      Wg <- private$X*weight_array
      private$O <- colSums(Wg) / colSums(private$X)
      private$O[is.nan(private$O)] <- 0
    },
    generate_rewards = function(n) {
      rwrd_n <- self$k * n
      if (self$reward_family == 'Bernoulli') {
        private$R <- round((runif(rwrd_n) + private$O) / 2)
      } else if (self$reward_family == 'Gaussian') {
        private$R <- (rnorm(rwrd_n, self$reward_means, self$reward_stds) + private$O) / 2
      } else if (self$reward_family == 'Poisson') {
        private$R <- (rpois(rwrd_n, self$reward_means) + private$O) / 2
      }
    },
    context_to_list = function(t) {
      if (self$precaching) idx <- t else idx <- 1
      list(
        k = self$k,
        d = self$d,
        X = private$X[,, idx]
      )
    },
    reward_to_list = function(t, action) {
      if (self$precaching) idx <- t else idx <- 1
      list(
        reward = private$R[action$choice, idx],
        optimal_reward_value = as.double(private$R[max_in(private$O[, idx]), idx])
      )
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
