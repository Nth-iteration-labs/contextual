#' @export
Bandit <- R6::R6Class(
  portable = TRUE,
  class    = FALSE,
  public   = list(
    k           = NULL,  # Number of arms (integer, required)
    d           = NULL,  # Dimension of context feature vector (integer, required)
    unique      = NULL,  # Vector of arm indices of unique context features (vector, optional)
    shared      = NULL,  # Vector of arm indices of context features shared between arms (vector, optional)
    precaching  = FALSE, # Pregenerate context & reward matrices? (boolean, required)
    class_name  = "Bandit",
    initialize  = function() {
      # Initialize Bandit. Set self$d and self$k here.
    },
    post_initialization = function() {
      # Called after setting seed, but before iterating over T. Do random generation here.
      invisible(self)
    },
    get_context = function(t) {
      stop("Bandit subclass needs to implement bandit$get_context()", call. = FALSE)
      # Return a list with self$k, self$d and, where applicable, the self$d x self$k context Matrix X.
      list(X = context, k = arms, d = features)
    },
    get_reward = function(t, context, action) {
      stop("Bandit subclass needs to implement bandit$get_reward()", call. = FALSE)
      # Return a list with the reward and, if known, the reward of the best arm.
      list(reward = reward_for_choice_made, optimal = optimal_reward_value)
    },
    generate_bandit_data = function(n) {
      # Called when precaching is TRUE. Pregenerate contexts and rewards here.
      stop("Bandit subclass needs to implement bandit$generate_cache()
           when bandit$precaching is TRUE.", call. = FALSE)
    },
    close = function() {
      # called on object destruction
    }
  )
)

#' Bandit
#'
#' R6 class \code{Bandit} is the parent of all \code{\{contextual\}} \code{Bandit} subclasses.
#' In \code{\{contextual\}}, \code{Bandits} are responsible for the generation of contexts and rewards.
#'
#' On initialisation, any \code{Bandit} subclass has to define the number of arms \code{self$k}
#' and the number of contextual feature dimensions \code{self$d}.
#'
#' \code{Bandit} subclasses are also responsible for returning \code{self$d x self$k} context matrices
#' through \code{get_context()}, and returning rewards through \code{get_reward()}.
#'
#' \code{Bandit} subclasses have option to (pre-)generate these values in \code{generate_bandit_data()}.
#'
#' @name Bandit
#' @aliases post_initialization get_context generate_bandit_data
#'
#' @importFrom R6 R6Class
#'
#' @section Usage:
#' \preformatted{
#'   policy <- Bandit$new()
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new()}}{ Generates and instantializes a new \code{Bandit} instance. }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step t.
#'      }
#'      returns a named list \code{list(k = n_arms, d = n_features, X = context)}
#'      containing the current \code{d x k} dimensional \code{context} matrix \code{X},
#'      the number of arms \code{k} and the number of features \code{d}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step t.
#'          \item \code{context}: list, containing \code{context$X} (d x k context matrix) \code{context$d}
#'          (number of context feaures) and \code{context$k} (number of arms).
#'          \item \code{action}:  list, containing \code{policy}'s \code{action$choice}.
#'      }
#'      returns:
#'      \itemize{
#'          \item \code{list(reward, optimal)} with the \code{reward} awarded to \code{action$choice}, and,
#'          if available, an \code{optimal} (or "oracle") reward.
#'      }
#'  }
#'
#'   \item{\code{post_initialization()}}{
#'      Called right after \code{Simulator} sets its seed,
#'      but before it starts iterating over all time steps \code{t} in T.
#'      If you need to set random values that remain available throughout the life of a \code{Bandit}
#'      instance, this is the place to do so.
#'   }
#'
#'   \item{\code{generate_bandit_data()}}{
#'      Helper function called before \code{Simulator} starts iterating over all time steps \code{t} in T.
#'      This function is only called when \code{bandit$precaching} is \code{TRUE} (default \code{FALSE}).
#'      Pregenerate \code{contexts} and \code{rewards} here.
#'   }
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
