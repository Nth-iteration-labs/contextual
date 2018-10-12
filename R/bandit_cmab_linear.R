#' @export
ContextualLinearBandit <- R6::R6Class(
  "ContextualLinearBandit",
  inherit = Bandit,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    sigma   = NULL,
    binary  = NULL,
    class_name = "ContextualLinearBandit",
    initialize  = function(k, d, sigma = 0.1, binary_rewards = FALSE) {
      self$k                                    <- k
      self$d                                    <- d
      self$sigma                                <- sigma
      self$binary                               <- binary_rewards
    },
    post_initialization = function() {
      self$betas                                <- matrix(runif(self$d*self$k, -1, 1), self$d, self$k)
      self$betas                                <- self$betas / norm(self$betas, type = "2")
    },
    get_context = function(t) {
      X                                         <- rnorm(self$d)
      reward_vector                             <- X %*% self$betas
      X                                         <- matrix(X, self$d,self$k)
      reward_vector                             <- reward_vector + rnorm(self$k, sd = self$sigma)

      if (isTRUE(self$binary)) {
        self$rewards                            <- rep(0,self$k)
        self$rewards[which_max_tied(reward_vector)] <- 1
      } else {
        self$rewards                            <- reward_vector
      }
      context <- list(
        k = self$k,
        d = self$d,
        X = X
      )
    },
    get_reward = function(t, context_common, action) {
      rewards        <- self$rewards
      optimal_arm    <- which_max_tied(rewards)
      reward         <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)

#' Bandit: ContextualLinearBandit
#'
#' Samples data from linearly parameterized arms.
#'
#' The reward for context X and arm j is given by X^T beta_j, for some latent
#' set of parameters {beta_j : j = 1, ..., k}. The beta's are sampled uniformly
#' at random, the contexts are Gaussian, and sigma-noise is added to the rewards.
#'
#' @name ContextualLinearBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- ContextualLinearBandit$new(k, d, sigma = 0.1, binary_rewards = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'
#'   \item{\code{k}}{
#'      integer; number of bandit arms
#'   }
#'  \item{\code{d}}{
#'      integer; number of contextual features
#'   }
#'   \item{\code{sigma}}{
#'      numeric; standard deviation of the additive noise. Set to zero for no noise. Default is \code{0.1}
#'   }
#'   \item{\code{binary_rewards}}{
#'      logical; when set to \code{FALSE} (default) ContextualLinearBandit generates Gaussian rewards.
#'      When set to \code{TRUE}, rewards are binary (0/1).
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(k, d, sigma = 0.1, binary_rewards = FALSE)}}{ generates and
#'   instantializes a new \code{ContextualLinearBandit} instance. }
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
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward} and, where computable,
#'         \code{reward$optimal} (used by "oracle" policies and to calculate regret).
#'  }
#'
#'   \item{\code{post_initialization()}}{
#'        initializes \code{d x k} beta matrix.
#'   }
#
#' }
#'
#' @references
#'
#' Riquelme, C., Tucker, G., & Snoek, J. (2018). Deep Bayesian Bandits Showdown: An Empirical Comparison of Bayesian Deep Networks for Thompson Sampling. arXiv preprint arXiv:1802.09127.
#'
#' Implementation follows \url{https://github.com/tensorflow/models/tree/master/research/deep_contextual_bandits}
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' horizon       <- 800L
#' simulations   <- 30L
#'
#' bandit        <- ContextualLinearBandit$new(k = 5, d = 5)
#'
#' agents        <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#'                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation     <- Simulator$new(agents, horizon, simulations)
#' history        <- simulation$run()
#'
#' plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "right")
#' }
NULL

