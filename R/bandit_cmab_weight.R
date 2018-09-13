#' @export
ContextualWeightBandit <- R6::R6Class(
  inherit = Bandit,
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
    sum_weights   = NULL,
    weights   = NULL,
    class_name = "ContextualWeightBandit",
    initialize   = function( reward_family = 'Bernoulli', reward_means = 4.0, reward_stds = 1.0,
                             weights = NULL, precaching  = TRUE, sum_weights = FALSE ) {

      if (!(reward_family %in% c("Bernoulli","Gaussian","Poisson"))) {
        stop('Reward family needs to be one of "Bernoulli", "Gaussian" or "Poisson".' , call. = FALSE)
      }

      if (is.vector(self$weights)) self$weights <- matrix(self$weights, nrow = 1L)
      if (!is.null(weights)) self$set_weights(weights)

      private$X <- array(1, dim = c(self$d, self$k, 1))
      self$has_cache            <- FALSE
      self$precaching           <- precaching
      self$reward_family        <- reward_family
      self$reward_means         <- reward_means
      self$reward_stds          <- reward_stds
      self$sum_weights          <- sum_weights
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
    generate_bandit_data = function(n = 1L, silent = TRUE ) {
      if (!silent) message("Precaching bandit" )
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
    context_list = list(),
    reward_list = list(),
    generate_context = function(n = 1L) {
      # private$X <- array(sample(c(0, 1), replace = TRUE,
                           # size = self$d * self$k * n), dim = c(self$d, self$k, n))
      private$X <- array(sample(c(0, 1), replace = TRUE, size = self$d * n), dim = c(self$d, self$k, n))
      private$X
    },
    generate_weights = function(n) {
      weight_array  <- array(t(matrix(private$W, self$k , self$d, byrow = TRUE )), dim = c(self$d, self$k, n))
      Wg <- private$X*weight_array
      private$O <- colSums(Wg)
      if (!isTRUE(self$sum_weights)) private$O <- private$O / self$d
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
      k <- self$k
      list(
        k = self$k,
        d = self$d,
        X = private$X[,, idx],
        o = which_max_tied(private$O[, idx])
      )
    },
    reward_to_list = function(t, action) {
      if (self$precaching) idx <- t else idx <- 1
      list(
        reward = private$R[action$choice, idx],
        optimal_reward_value = as.double(private$R[which_max_tied(private$O[, idx]), idx])
      )
    }
  )
)

#' Bandit: ContextualWeightBandit
#'
#' Contextual extension of BasicBernoulliBandit. Employs a user defined weight matrix for reward generation.
#'
#' \code{ContextualWeightBandit} illustrates different types of rewards (\code{Bernoulli}, \code{Gaussian} and
#' \code{Poisson}) and the \code{contextual} context and reward precaching mechanism.
#'
#' The \code{d x k} weight matrix determines the number of simulated arms \code{k} and contextual
#' features \code{d}. For every \code{t}, some context features (rows) are sampled at random. Rewards for an
#' arm are determined by its column mean in the weight matrix
#'
#' @name ContextualWeightBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- ContextualWeightBandit$new(reward_family = "Bernoulli", reward_means = 4.0, reward_stds = 1.0, weights = NULL, precaching  = TRUE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'
#'   \item{\code{reward_family}}{
#'      integer; number of bandit arms
#'   }
#'  \item{\code{reward_means}}{
#'      integer; number of contextual features
#'   }
#'   \item{\code{reward_stds}}{
#'      logical; if TRUE (default) it adds a constant (1.0) dimension to each context X at the end.
#'   }
#'   \item{\code{weights}}{
#'      integer; number of bandit arms
#'   }
#'  \item{\code{precaching}}{
#'      logical; determines if the bandit precaches all contexts and rewards. Faster if TRUE (default).
#'   }
#'   \item{\code{sum_weights}}{
#'      logical; determines if the bandit precaches all contexts and rewards. Faster if TRUE (default).
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(k, d, intercept = NULL)}}{
#'   generates and instantializes a new \code{Bandit} instance.
#'   For arguments, see Argument section above.
#'
#'   }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the current \code{d x k} dimensional matrix \code{context$X},
#'      the number of arms \code{context$k} and the number of features \code{context$d}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context feaures)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward}
#'  }
#'
#'   \item{\code{post_initialization()}}{
#'        initialzes \code{d x k} beta matrix.
#'   }
#
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
#' horizon            <- 100L
#' simulations        <- 100L
#' context_weights    <- matrix(  c(0.4, 0.2, 0.4,
#'                                  0.3, 0.4, 0.3,
#'                                  0.1, 0.8, 0.1),  nrow = 3, ncol = 3, byrow = TRUE)
#'
#' bandit             <- ContextualWeightBandit$new(weights = context_weights, precaching = FALSE)
#'
#' agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#'                             Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
#' history            <- simulation$run()
#'
#' plot(history, type = "cumulative")
#'
#'
#' }
NULL

