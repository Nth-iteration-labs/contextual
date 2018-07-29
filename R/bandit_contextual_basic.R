# Samples data from linearly parameterized arms.
#
# The reward for context X and arm j is given by X^T beta_j, for some latent
# set of parameters {beta_j : j = 1, ..., k}. The beta's are sampled uniformly
# at random, the contexts are Gaussian, and sigma-noise is added to the rewards.
#
# Args:
# num_contexts: Number of contexts to sample.
# dim_context: Dimension of the contexts.
# num_actions: Number of arms for the multi-armed bandit.
# sigma: Standard deviation of the additive noise. Set to zero for no noise.
#
# Returns:
# data: A [n, d+k] numpy array with the data.
# betas: Latent parameters that determine expected reward for each arm.
# opt: (optimal_rewards, optimal_actions) for all contexts.

#' @export
BasicContextualBandit <- R6::R6Class(
  "BasicContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    betas   = NULL,
    sigma   = NULL,
    binary  = NULL,
    class_name    = "BasicContextualBandit",
    precaching = FALSE,
    initialize  = function(k, d, sigma = 0.1, binary_rewards = TRUE) {
      self$k                                    <- k
      self$d                                    <- d
      self$sigma                                <- sigma
      self$binary                               <- binary_rewards
    },
    post_initialization = function() {
      self$betas                                <- matrix(runif(self$d*self$k, -1, 1), self$d, self$k)
      self$betas                                <- self$betas / norm(self$betas, type = "F")
    },
    get_context = function(t) {

      context                                   <- rnorm(self$d)
      reward_vector                             <- context %*% self$betas
      context                                   <- matrix(context, self$d,self$k)
      reward_vector                             <- reward_vector + rnorm(self$k, sd = self$sigma)

      if (isTRUE(self$binary)) {
        self$rewards                            <- rep(0,self$k)
        self$rewards[which.max(reward_vector)]  <- 1
      } else {
        self$rewards                            <- reward_vector
      }

      context_list <- list(
        k = self$k,
        d = self$d,
        d_disjoint = self$d_disjoint,
        X = context
      )
      context_list
    },
    get_reward = function(t, context_common, action) {
      rewardlist <- list(
        reward                   = self$rewards[action$choice],
        optimal_reward_value     = self$rewards[which.max(self$rewards)]
      )
      rewardlist
    }
  )
)

#' Bandit: BasicContextualBandit
#'
#' BasicContextualBandit intro
#'
#' Following Allen Day DataGenerator
#'
#' @name BasicContextualBandit
#' @family context_commonual subclasses
#'
#' @section Usage:
#' \preformatted{b <- BasicContextualBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{BasicContextualBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new BasicContextualBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core context_commonual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
NULL
