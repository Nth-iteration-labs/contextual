 #' @export
BasicGaussianBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    mu_per_arm = NULL,
    sigma_per_arm = NULL,
    class_name = "BasicGaussianBandit",
    initialize = function(mu_per_arm, sigma_per_arm) {
      self$mu_per_arm      <- mu_per_arm
      self$sigma_per_arm   <- sigma_per_arm
      self$k               <- length(self$mu_per_arm)
    },
    get_context = function(t) {
      context <- list(
        k = self$k
      )
    },
    get_reward = function(t, context, action) {
      rewards <- rnorm(self$k, self$mu_per_arm, self$sigma_per_arm)
      optimal_arm    <- which_max_tied(self$mu_per_arm)
      reward         <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = self$mu_per_arm[optimal_arm]
      )
    }
  )
)

#' Bandit: BasicGaussianBandit
#'
#' Context-free Gaussian multi-armed bandit.
#'
#' Simulates \code{k} Gaussian arms where each arm models the reward as a normal
#' distribution with provided mean \code{mu} and standard deviation \code{sigma}.
#'
#' @name BasicGaussianBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- BasicGaussianBandit$new(mu_per_arm, sigma_per_arm)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{mu_per_arm}}{
#'      numeric vector; mean \code{mu} for each of the bandit's \code{k} arms
#'   }
#'   \item{\code{sigma_per_arm}}{
#'      numeric vector; standard deviation of additive Gaussian noise for each of
#'      the bandit's \code{k} arms. Set to zero for no noise.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(mu_per_arm, sigma_per_arm)}}{ generates and instantializes a
#'   new \code{BasicGaussianBandit} instance. }
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
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' horizon            <- 100
#' sims               <- 100
#'
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'
#' bandit             <- BasicGaussianBandit$new(c(0,0,1), c(1,1,1))
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(agent, horizon, sims)$run()
#'
#' plot(history, type = "cumulative", regret = TRUE)
#'
#' }
NULL


