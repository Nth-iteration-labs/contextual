 #' @export
MabWeightBandit <- R6::R6Class(
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    R = NULL,
    X = NULL,
    W = NULL
  ),
  public = list(
    class_name = "MabWeightBandit",
    initialize   = function(weights = NULL) {
      if (!is.null(weights)) self$set_weights(weights)
      private$X <- array(1, dim = c(self$d, self$k, 1))
    },
    get_weights = function() {
      private$W
    },
    set_weights = function(local_W) {
      if (is.vector(local_W)) private$W <- matrix(local_W, nrow = 1L)
      self$d <- as.integer(dim(private$W)[1])
      self$k <- as.integer(dim(private$W)[2])
      invisible(private$W)
    },
    get_context = function(t) {
      context <- list(
        k = self$k,
        d = self$d
      )
      context
    },
    get_reward = function(t, context, action) {
      private$R  <- as.double(matrix(runif(self$k) < self$get_weights(), self$k, self$d))
      reward <- list(
        reward                   = private$R[action$choice],
        optimal_reward_value     = private$R[which.max(private$R)]
      )
      reward
    }
  )
)

#' Bandit: MabWeightBandit
#'
#' Context-free synthetic multi-armed bandit.
#'
#' Simulates \code{k} Bernoulli arms each generating binary rewards with a random uniform probability set by
#' \code{MabWeightBandit} weight vector.
#'
#' @name MabWeightBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- MabWeightBandit$new(weights = NULL)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{weights}}{
#'      integer; number of bandit arms
#'   }
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
#' Bandit subclass examples: \code{\link{MabWeightBandit}}, \code{\link{ContextualWeightBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
#' policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'
#' bandit             <- MabWeightBandit$new(weights = c(0.6, 0.1, 0.1))
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(
#'   agents = agent,
#'   horizon = 100,
#'   simulations = 100
#' )$run()
#'
#' plot(history, type = "cumulative", regret = TRUE, ci = "ci",  traces_max = 100, traces_alpha = 0.1,
#'      traces = TRUE, smooth = FALSE, interval = 1)
#' }
NULL


