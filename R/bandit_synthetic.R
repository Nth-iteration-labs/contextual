# create from scratch if not cache, otherwise do cahce
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

    d             = NULL,
    k             = NULL,
    reward_means  = NULL,
    reward_stds   = NULL,
    reward_family = NULL,
    precache      = NULL,
    has_cache     = NULL,
    not_zero_features = NULL,
    random_one_feature = NULL,

    initialize   = function(
      reward_family        = 'Bernoulli',
      reward_means         = 4.0,
      reward_stds          = 1.0,
      weights              = NULL,
      precache             = TRUE,
      not_zero_features    = TRUE,
      random_one_feature   = FALSE
    ) {

      if (!(reward_family %in% c("Bernoulli","Gaussian","Poisson"))) {
        stop('Reward family needs to be one of "Bernoulli", "Gaussian" or "Poisson".' , call. = FALSE)
      }

      super$initialize(weights)

      self$has_cache            <- FALSE
      self$is_precaching        <- precache
      self$reward_family        <- reward_family
      self$reward_means         <- reward_means
      self$reward_stds          <- reward_stds
      self$not_zero_features    <- not_zero_features
      self$random_one_feature    <- random_one_feature
    },
    get_context = function(t) {
      if (self$is_precaching) {
        private$context_to_list(t)
      } else {
        self$generate_bandit_data(n = 1L)
        private$context_to_list(t = 1)
      }
    },
    get_reward = function(action, t) {
      private$reward_to_list(action, t)
    },

    generate_bandit_data = function(n = 1L,
                                    silent = TRUE ) {
      if (!silent) message("Precaching bandit" )
      private$X <- matrix(0, n , self$d)
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
      if (self$not_zero_features | self$random_one_feature) {
        private$X[cbind(1L:n, sample(self$d, n, replace = TRUE))] <- 1
      }
      if (!self$random_one_feature) {
        private$X <- matrix((private$X | matrix(sample(c(0, 1), replace = TRUE, size = n * self$d),  n, self$d)), n, self$d)
      }
      mode(private$X) <- 'integer'
    },

    generate_oracle = function(n) {
      Wg <-
        sweep(array(t(matrix(
          private$W, self$k , self$d
        )), dim = c(self$d, self$k, n)),
        3,
        private$X,
        FUN = "*",
        check.margin = FALSE)
      private$O <-
        t(t(colSums(Wg)) / as.vector(rowSums(private$X)))
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

#W <- repmat(t(matrix(private$W, self$k , self$d)),n,1) *
#     as.vector(matrix(t(private$X), n*d, 1 , byrow = TRUE))
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
