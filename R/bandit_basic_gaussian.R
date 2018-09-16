 #' @export
BasicGaussianBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
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
      optimal_arm    <- which.max(rewards)
      reward  <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
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
#' @importFrom R6 R6Class
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
#'      numeric vector; standard deviation \code{sigma} for each of
#'      the bandit's \code{k} arms
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(mu_per_arm, sigma_per_arm)}}{
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
#
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicGaussianBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
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


