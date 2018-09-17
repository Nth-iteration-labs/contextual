#' @export
ContextualHybridBandit <- R6::R6Class(
  "ContextualHybridBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    betas_s = NULL,                                                 ## regression betas shared over all arms
    betas_u = NULL,                                                 ## regression betas unique per arm
    s       = NULL,                                                 ## nr shared features/betas
    u       = NULL,                                                 ## nr unique features/betas
    sigma   = NULL,                                                 ## standard deviation of noise

    class_name = "ContextualHybridBandit",
    precaching = FALSE,
    initialize  = function(k, shared_features, unique_features, sigma = 1.0) {

      assert_count(shared_features, positive = TRUE)
      assert_count(unique_features, positive = TRUE)
      assert_number(sigma, lower = 0)

      self$sigma   <- sigma
      self$k       <- k                                             ## nr of arms
      self$s       <- shared_features                               ## nr shared features/betas
      self$u       <- unique_features                               ## nr unique/disjoint features/betas
      self$d       <- self$u + self$s                               ## total number of features

      self$shared  <- c(1:self$s)
      self$unique  <- c((self$s+1):(self$u+self$s))
    },
    post_initialization = function() {
      self$betas_s <- runif(self$s,0,1/(self$u+1))                  ## generate unique/disjoint features/betas
      self$betas_u <- matrix(runif(self$u*self$k), self$u, self$k)  ## generate shared features/betas
    },
    get_context = function(t) {
      X <- matrix(runif(self$d*self$k, 0, 1), self$d, self$k)
      context_list <- list(
        k = self$k,
        d = self$d,
        unique = self$unique,
        shared = self$shared,
        X = X
      )
      context_list
    },
    get_reward = function(t, context, action) {
      betas        <- c(self$betas_s, self$betas_u[,action$choice])
      trb          <- betas%*%context$X[,action$choice]
      trb          <- trb + rnorm(1,0,self$sigma)
      reward       <- rbinom(1,1,1/(1+exp(-trb)))
      rewardlist   <- list(
        reward                   = reward,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)

#' Bandit: ContextualHybridBandit
#'
#' Extension of \code{ContextualLogitBandit} modeling hybrid rewards with a combination of unique (or
#' "disjoint") and shared contextual features.
#'
#' @name ContextualHybridBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- ContextualHybridBandit$new(k, d, intercept = TRUE)
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
#'     generates and instantializes a new \code{Bandit} instance.
#'     For arguments, see Argument section above.
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
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
#' horizon       <- 800L
#' simulations   <- 100L
#'
#' bandit        <- ContextualHybridBandit$new(k = 100, shared_features = 10, unique_features = 2)
#'
#' agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5,
#'                                                    R=0.01, epsilon=0.5), bandit),
#'                       Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
#'                       Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
#'                       Agent$new(ContextualEpochGreedyPolicy$new(8), bandit),
#'                       Agent$new(LinUCBHybridOptimizedPolicy$new(0.6), bandit),
#'                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))
#'
#' simulation     <- Simulator$new(agents, horizon, simulations)
#' history        <- simulation$run()
#'
#' plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")
#' }
NULL
