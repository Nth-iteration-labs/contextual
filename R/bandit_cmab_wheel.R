#' @export
ContextualWheelBandit <- R6::R6Class(
  "ContextualWheelBandit",
  inherit = Bandit,
  class = FALSE,
  public = list(
    rewards = NULL,
    delta = NULL,
    mean_v = NULL,
    std_v = NULL,
    mu_large = NULL,
    std_large = NULL,
    class_name = "ContextualWheelBandit",
    initialize  = function(delta, mean_v, std_v, mu_large, std_large) {
      self$k                                    <- 5
      self$d                                    <- 2
      self$delta                                <- delta
      self$mean_v                               <- mean_v         # per arm
      self$std_v                                <- std_v          # per arm
      self$mu_large                             <- mu_large
      self$std_large                            <- std_large
    },
    get_context = function(t) {

      # sample uniform contexts in unit ball

      repeat{
        X                                 <- runif(self$d, -1, 1)
        if(norm(as.matrix(X),"2") <= 1) {
          break
        }
      }
      # sample rewards

      self$rewards                              <- rnorm(self$k,self$mean_v,self$std_v)

      if (norm(as.matrix(X),"2") >= self$delta) {
        r_big                                   <- rnorm(1,self$mu_large, self$std_large)
        if (X[1] > 0) {
          if (X[2] > 0) {
            self$rewards[1]                     <- r_big
          } else {
            self$rewards[2]                     <- r_big
          }
        } else {
          if (X[2] > 0) {
            self$rewards[3]                     <- r_big
          } else {
            self$rewards[4]                     <- r_big
          }
        }
      }

      context                                   <- list(
        k = self$k,
        d = self$d,
        X = X
      )

      context
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


#' Bandit: ContextualWheelBandit
#'
#' Samples from Wheel bandit game.
#'
#' The Wheel bandit game offers an artificial problem where the need for exploration is smoothly parameterized
#' through exploration parameter \code{delta}.
#'
#' In the game, contexts are sampled uniformly at random from a unit circle divided into one central and four
#' edge areas for a total of \code{k = 5} possible actions. The central area offers a random normal sampled
#' reward independent of the context, in contrast to the outer areas which offer a random normal sampled
#' reward dependent on a \code{d = 2} dimensional context.
#'
#' For more information, see \url{https://arxiv.org/abs/1802.09127}.
#'
#' @name ContextualWheelBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- ContextualWheelBandit$new(delta, mean_v, std_v, mu_large, std_large)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'
#'   \item{\code{delta}}{
#'      numeric; exploration parameter: high reward in one region if norm above delta.
#'   }
#'  \item{\code{mean_v}}{
#'      numeric vector; mean reward for each action if context norm is below delta.
#'   }
#'   \item{\code{std_v}}{
#'      numeric vector; gaussian reward sd for each action if context norm is below delta.
#'   }
#'   \item{\code{mu_large}}{
#'     numeric; mean reward for optimal action if context norm is above delta.
#'   }
#'   \item{\code{std_large}}{
#'     numeric; standard deviation of the reward for optimal action if context norm is above delta.
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(delta, mean_v, std_v, mu_large, std_large)}}{ generates and instantializes a
#'   new \code{ContextualWheelBandit} instance. }
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
#' horizon       <- 1000L
#' simulations   <- 10L
#'
#' delta         <- 0.95
#' num_actions   <- 5
#' context_dim   <- 2
#' mean_v        <- c(1.0, 1.0, 1.0, 1.0, 1.2)
#' std_v         <- c(0.05, 0.05, 0.05, 0.05, 0.05)
#' mu_large      <- 50
#' std_large     <- 0.01
#'
#' bandit        <- ContextualWheelBandit$new(delta, mean_v, std_v, mu_large, std_large)
#' agents        <- list(Agent$new(UCB1Policy$new(), bandit),
#'                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation     <- Simulator$new(agents, horizon, simulations)
#' history        <- simulation$run()
#'
#' plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")
#' }
NULL
