#' @export
ContextualLogitBandit <- R6::R6Class(
  "ContextualLogitBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    beta   = NULL,
    intercept = NULL,
    class_name = "ContextualLogitBandit",
    precaching = FALSE,
    initialize  = function(k, d, intercept = TRUE) {
      self$k              <- k
      self$d              <- d
      self$intercept      <- intercept
    },
    post_initialization = function() {
      if (self$intercept && self$d > 1) {
        self$beta   <- c(rnorm(self$d-1,0,1),1)
      } else {
        self$beta   <- c(rnorm(self$d,0,1))
      }
    },
    get_context = function(t) {
      X <- matrix(runif(self$d*self$k, 0, 1), self$d, self$k)
      context <- list(
        k = self$k,
        d = self$d,
        X = X
      )
      context
    },
    get_reward = function(t, context, action) {
      X          <- context$X                             # context matrix
      d          <- context$d                             # number of context features
      arm        <- action$choice                         # arm chosen by policy

      z          <- as.vector(self$beta%*%X)              # compute linear predictor
      pr         <- 1/(1+exp(-z))                         # inverse logit transform of linear predictor
      reward     <- rbinom(d,1,pr)                        # binary rewards from the Bernoulli distribution

      reward <- list(
        reward                   = reward[arm],
        optimal_reward_value     = reward[which.max(reward)]
      )
      reward
    }
  )
)

#' ContextualLogitBandit
#'
#' Samples data from a basic logistic regression model.
#'
#' ContextualLogitBandit linear predictors are generated from the dot product of a random \code{d} dimensional
#' normal weight vector and uniform random \code{d x k} dimensional context matrices with equal weights per
#' arm. This product is then inverse-logit transformed to generate \code{k} dimensional binary (0/1) reward
#' vectors by randomly sampling from a Bernoulli distribution.
#'
#' @name ContextualLogitBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- ContextualLogitBandit$new(k, d, intercept = TRUE)
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
#'   \item{\code{intercept}}{
#'      logical; if TRUE (default) it adds a constant (1.0) dimension to each context X at the end.
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
#' horizon       <- 800L
#' simulations   <- 30L
#'
#' bandit        <- ContextualLogitBandit$new(k = 5, d = 5, intercept = TRUE)
#'
#' agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5,
#'                                                        R=0.01, epsilon=0.5), bandit),
#'                       Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#'                       Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
#'                       Agent$new(ContextualEpochGreedyPolicy$new(8), bandit),
#'                       Agent$new(LinUCBHybridOptimizedPolicy$new(0.6), bandit),
#'                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
#' history        <- simulation$run()
#'
#' plot(history, type = "cumulative", regret = FALSE,
#'               rate = TRUE, legend_position = "right")
#' }
NULL
