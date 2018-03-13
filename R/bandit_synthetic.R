#' @export
SyntheticBandit <- R6::R6Class(
  "SyntheticBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  active = list(
    is_precaching = function(value) {
      if (missing(value)) {
        private$precaching
      } else {
        private$precaching <- value
      }
    }
  ),
  public = list(
    k             = NULL, # n of arms
    d             = NULL, # n of features
    d_context     = NULL, # subset n of context features
    d_arms        = NULL, # subset n of arm features
    reward_means  = NULL,
    reward_stds   = NULL,
    reward_family = NULL,
    precache      = NULL,
    has_cache     = NULL,

    not_zero_features = NULL,
    random_one_feature = NULL,

    context_weights   = NULL,
    arm_weights       = NULL,
    arm_mask          = NULL,

    initialize   = function(
      reward_family        = 'Bernoulli',
      reward_means         = 4.0,
      reward_stds          = 1.0,

      context_weights      = NULL,
      arm_weights          = NULL,
      arm_mask             = NULL,

      precache             = TRUE,
      not_zero_features    = TRUE,
      random_one_feature   = FALSE

    ) {

      if (!(reward_family %in% c("Bernoulli","Gaussian","Poisson"))) {
        stop('Reward family needs to be one of "Bernoulli", "Gaussian" or "Poisson".' , call. = FALSE)
      }

      self$arm_weights          <- arm_weights
      self$arm_mask             <- arm_mask

      self$context_weights      <- context_weights

      if (is.vector(self$context_weights)) self$context_weights <- matrix(self$context_weights, nrow = 1L)
      if (is.vector(self$arm_weights)) self$arm_weights <- matrix(self$arm_weights, nrow = 1L)

      self$d_context <- dim(self$context_weights)[1]
      self$d_arms <- dim(self$arm_weights)[1]

      weights = rbind(self$context_weights, self$arm_weights)

      if (!is.null(self$arm_weights) & is.null(self$arm_mask)) {
        if (dim(weights)[1] == 1) {
          self$arm_mask <- matrix(rep(1,length(self$arm_weights)),nrow = 1L  )
        } else {
          stop('Please set arm_mask for arm_weights.' , call. = FALSE)
        }
      }
      super$initialize(weights)

      self$has_cache            <- FALSE
      self$is_precaching        <- precache
      self$reward_family        <- reward_family
      self$reward_means         <- reward_means
      self$reward_stds          <- reward_stds
      self$not_zero_features    <- not_zero_features
      self$random_one_feature   <- random_one_feature
    },
    get_context = function(t) {
      if (self$is_precaching) {
        private$context_to_list(t)
      } else {
        self$generate_bandit_data(n = 1L)
        private$context_to_list(t = 1)
      }
    },
    do_action = function(action, t) {
      private$reward_to_list(action, t)
    },

    generate_bandit_data = function(n = 1L,
                                    silent = TRUE ) {
      if (!silent) message("Precaching bandit" )
      private$X <- array(0, dim = c(self$d, self$k, n))
      private$O <- matrix(0, self$k, n)
      private$R <- matrix(0, self$k, n)
      private$generate_context(n)
      private$generate_oracle(n)
      private$generate_rewards(n)
      self$has_cache <- TRUE
    }
  ),
  private = list(
    generate_context = function(n = 1L) {
      if (!is.null(self$context_weights)) {
        context_d <- dim(self$context_weights)[1]
        context_mask <- matrix(0, n , context_d)
        if (self$not_zero_features | self$random_one_feature) {
          context_mask[cbind(1L:n, sample(context_d, n, replace = TRUE))] <- 1
        }
        if (!self$random_one_feature) {
          context_mask <- matrix((context_mask | matrix(sample(c(0, 1), replace = TRUE, size = n * context_d),  n, context_d)), n, context_d)
        }
        mode(context_mask) <- 'integer'
        private$X <- array(0, dim = c(self$d, self$k, n))
        for (i in 1:n) {
          context_mask_to_matrix <- matrix( context_mask[i,], context_d, self$k)
          private$X[,,i] <- rbind(context_mask_to_matrix, self$arm_mask)
        }
      } else {
        private$X <- array(0, dim = c(self$d, self$k, n))
        for (i in 1:n) {
          private$X[,,i] <- self$arm_mask
        }
      }
      private$X
    },
    generate_oracle = function(n) {
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
