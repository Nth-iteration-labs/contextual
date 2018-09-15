#' @export
ContextualBernoulliBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    d             = NULL,
    k             = NULL,
    precaching    = NULL,
    sum_weights   = NULL,
    weights       = NULL,
    class_name = "ContextualBernoulliBandit",
    initialize   = function(weights = NULL, precaching  = TRUE, sum_weights = FALSE) {
      if (is.vector(self$weights)) self$weights <- matrix(self$weights, nrow = 1L)
      if (!is.null(weights)) self$set_weights(weights)
      private$X <- array(1, dim = c(self$d, self$k, 1))
      self$precaching           <- precaching
      self$sum_weights          <- sum_weights
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
    set_weights = function(local_W) {
      if (is.vector(local_W)) private$W <- matrix(local_W, nrow = 1L)
      if (is.matrix(local_W)) private$W <- local_W
      self$d <- as.integer(dim(private$W)[1])
      self$k <- as.integer(dim(private$W)[2])
    },
    generate_bandit_data = function(n = 1L, silent = TRUE ) {
      if (!silent) message("Precaching bandit" )
      private$O <- matrix(0, self$k, n)
      private$R <- matrix(0, self$k, n)
      private$generate_context(n)
      private$generate_weights(n)
      private$generate_rewards(n)
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
      private$X <- array(sample(c(0, 1), replace = TRUE, size = self$d * n), dim = c(self$d, self$k, n))
    },
    generate_weights = function(n) {
      weight_array  <- array(t(matrix(private$W, self$k , self$d, byrow = TRUE)), dim = c(self$d, self$k, n))
      private$O <- colSums(private$X*weight_array)
      if (!isTRUE(self$sum_weights)) private$O <- private$O / colSums(private$X)
      private$O[is.nan(private$O)] <- 0
    },
    generate_rewards = function(n) {
      private$R <- round((runif( self$k * n) + private$O) / 2)
    },
    context_to_list = function(t) {
      if (self$precaching) idx <- t else idx <- 1
      list(k = self$k, d = self$d, X = private$X[,, idx])
    },
    reward_to_list = function(t, action) {
      if (self$precaching) idx <- t else idx <- 1
      list(
        reward = private$R[action$choice, idx],
        optimal_reward = as.double(private$R[which_max_tied(private$R[, idx]), idx]),
        optimal_arm = which_max_tied(private$R[, idx])
      )
    }
  )
)

#' Bandit: ContextualBernoulliBandit
#'
#' Contextual extension of BasicBernoulliBandit where c{d x k} dimensional matrix takes the
#' place of BasicBernoulliBandit's {k} dimensional probability vector. Here, each row {d} represents a feature
#' with {k} probability values per arms.
#'
#' For every \code{t}, ContextualBernoulliBandit samples from its {d} features or rows at random, then
#' generates rewards contingent on either the sum or mean {p} per column or arm for the selected features.
#'
#' Illustrates precaching of contexts and rewards.
#'
#' @name ContextualBernoulliBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- ContextualBernoulliBandit$new(weights = NULL, precaching  = TRUE, sum_weights = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{weights}}{
#'      numeric matrix; {d x k} dimensional matrix, where each row {d} represents a feature with {k}
#'      p values per arms.
#'   }
#'  \item{\code{precaching}}{
#'      logical; determines if the bandit precaches all contexts and rewards.
#'   }
#'   \item{\code{sum_weights}}{
#'      logical; determines whether ContextualBernoulliBandit takes the sum or the mean (default) of the
#'      available p values per arm before the bandit generates its rewards.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(weights = NULL, precaching  = TRUE, sum_weights = FALSE)}}{
#'      generates and instantializes a new \code{Bandit} instance.
#'      For arguments, see Argument section above.
#'   }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the number of arms as \code{context$k}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, with \code{context$k} (number of arms).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward}
#'  }
#'
#'   \item{\code{generate_bandit_data()}}{
#'      helper function called before \code{Simulator} starts iterating over all time steps \code{t} in T.
#'      This function is only called when \code{bandit$precaching} is \code{TRUE} (default \code{FALSE}).
#'      Pregenerates \code{contexts} and \code{rewards}.
#'  }
#'
#'
#'
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
#' bandit             <- ContextualBernoulliBandit$new(weights = context_weights, precaching = FALSE)
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

