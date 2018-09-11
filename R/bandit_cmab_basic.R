#' @export
ContextualBasicBandit <- R6::R6Class(
  "ContextualBasicBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    beta   = NULL,
    class_name = "ContextualBasicBandit",
    precaching = FALSE,
    initialize  = function(k, d) {
      self$k       <- k
      self$d       <- d
    },
    post_initialization = function() {
      self$beta   <- rnorm(self$d,0,1)
    },
    get_context = function(t) {
      X <- matrix(runif(self$d*self$k, 0, 1), self$d, self$k)
      context_list <- list(
        k = self$k,
        d = self$d,
        X = X
      )
      context_list
    },
    get_reward = function(t, context, action) {
      reward <- rbinom(1,1,1/(1+exp(-self$beta%*%context$X[,action$choice])))
      rewardlist <- list(
        reward                   = reward,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)

#' ContextualBasicBandit
#'
#' ContextualBasicBandit is a basic contextual synthetic bandit that takes a number of arms \code{k}
#' and contextual features \code{d} to pre-generate a \code{d} length vector \code{beta} sampled from
#' \code{rnorm(self$d,0,1)}.
#'
#' For each \code{t} in \code{T} it then generates respectively \code{d x k} uniformly random \code{context$X}
#' and a \code{reward} that is based on a binomial reward function that takes a dot product of \code{beta} and
#' \code{context$X}
#'
#' \code{Bandit} subclasses are also responsible for returning \code{self$d x self$k} context matrices
#' through \code{get_context()}, and returning rewards through \code{get_reward()}.
#'
#' \code{Bandit} subclasses have option to (pre-)generate these values in \code{generate_bandit_data()}.
#'
#' @name ContextualBasicBandit
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- ContextualBasicBandit$new(k, d)
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(k, d)}}{ Generates and instantializes a new \code{Bandit} instance. }
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
#'        Initialzes \code{d x k} beta matrix.
#'   }
#
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{MabWeightBandit}}, \code{\link{ContextualBasicBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#'
NULL
