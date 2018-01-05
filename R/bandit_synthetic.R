# create from scratch if not cache, otherwise do cahce
#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = AbstractBandit,
  portable = FALSE,
  class = FALSE,
  active = list(
    is_precaching = function(value) {
      if (missing(value)) {
        private$.is_precaching
      } else {
        private$.is_precaching <- value
      }
    }
  ),
  public = list(
    d             = NULL,
    k             = NULL,
    weight_stds   = NULL,
    weight_means  = NULL,
    reward_means  = NULL,
    reward_stds   = NULL,
    reward_type   = NULL,
    weight_distribution  = NULL,
    seed = NULL,
    precache = NULL,
    has_cache = NULL,
    initialize   = function(weight_distribution  = 'Uniform',
                            reward_type          = 'Bernoulli',
                            seed                 = 1L,
                            weight_stds          = NA,
                            weight_means         = NA,
                            reward_means         = NA,
                            reward_stds          = NA,
                            precache             = TRUE
                           ) {
      super$initialize()
      self$has_cache            <- FALSE                                        # true / false? version nr
      self$seed                 <- seed
      self$is_precaching        <- precache
      self$reward_type          <- reward_type
      self$weight_distribution  <- weight_distribution
    },
    get_context = function(t) {
      if (!self$is_precaching) {
        self$generate_cache(n = 1L, not_zero_features = TRUE, seed_counter = t)
        t = 1
      }
      self$context_to_list(t)
    },
    get_reward = function(action, t) {
      if (!self$is_precaching) t = 1
      self$reward_to_list(action, t)
    },
    generate_weights = function(k,
                                d = 1L,
                                mean = 3.0,
                                sd = 1.0) {
      set.seed(self$seed)
      self$k <- k
      self$d <- d
      if (self$weight_distribution == "Uniform") {
        private$.W <- matrix(runif(d * k), d, k)
      } else if (self$weight_distribution == "Normal") {
        private$.W <-
          matrix(rnorm(self$d * self$k, self$feature_means, self$feature_stds),
                 d,
                 k)
      }
      invisible(self)
    },
    generate_cache = function(n = 1L,
                              not_zero_features = TRUE,
                              seed_counter = 0L) {
      print("precaching bandit" )
      set.seed(self$seed + seed_counter)
      private$.X <- matrix(0, n , d)
      private$.O <- matrix(0, self$k, n)
      private$.R <- matrix(0, self$k, n)
      private$.generate_context(n, not_zero_features)
      private$.generate_oracle(n)
      private$.generate_rewards(n)
      self$has_cache = TRUE
    }
  ),
  private = list(
    .generate_context = function(n = 1L,
                                 not_zero_features = TRUE) {
      if (not_zero_features) {
        private$.X[cbind(1L:n, sample(self$d, n, replace = TRUE))] <- 1
      }
      private$.X <- matrix((private$.X |
                              matrix(
                                sample(c(0, 1),
                                       replace = TRUE,
                                       size = n * self$d),
                                n ,
                                self$d
                              )
                            ),
                           n,
                           self$d)
      mode(private$.X) <- 'integer'
    },
    .generate_oracle = function(n) {
      W <-
        sweep(array(t(matrix(
          private$.W, self$k , self$d
        )), dim = c(self$d, self$k, n)),
        3,
        private$.X,
        FUN = "*",
        check.margin = FALSE)
      private$.O <-
        t(t(colSums(W)) / as.vector(rowSums(private$.X)))
      private$.O[is.nan(private$.O)] <- 0
    },



    ############## this is more general!

    .generate_rewards = function(n) {
      if (self$reward_type == 'Bernoulli') {
        private$.R <- runif(self$k * n) < private$.O
      } else if (self$reward_type == 'Gaussian') {
        private$.R <-
          (rnorm(self$k * n, self$reward_means, self$reward_stds) + private$.O) / 2
      }
    }

  )
)

#W <- repmat(t(matrix(private$.W, self$k , self$d)),n,1) *
#     as.vector(matrix(t(private$.X),n*d,1 , byrow = TRUE))
#W <- array(t(W), dim = c(self$k , self$d, n))
#W <- aperm(W,c(2,1,3))

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
