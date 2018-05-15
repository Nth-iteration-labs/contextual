#' @export
Bandit <- R6::R6Class(
  "Bandit",
  portable = TRUE,
  class    = FALSE,
  public   = list(
    k             = NULL,  # Number of arms (integer)
    d             = NULL,  # Dimension of context feature vector (integer)
    precaching    = FALSE, # Pregenerate context & reward matrices? (boolean)
    initialize  = function() {
      # Initialize Bandit.
    },
    pre_calculate = function() {
      # Called after setting seed, but before iterating over T. Do random generation here.
    },
    get_context = function(t) {
      stop("Bandit subclass needs to implement bandit$get_context()", call. = FALSE)
      # Return a list with self$k, self$d and, where applicable, a context vector X.
      list(k = n_arms, d = n_features, X = context)
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
    }
  )
)

#' Bandit
#'
#' The R6 class \code{Bandit} is the parent class of all \code{Bandits} implemented in \code{\{contextual\}}.
#' Classes that extend the abstract superclass \code{Bandit} are responsible for both the generation
#' of \code{d} dimensional \code{context} vectors \code{X} and the \code{k} I.I.D. distributions
#' each generating a \code{reward} for each of its \code{k} arms at each time step \code{t}.
#' \code{Bandit} subclasses may (pre)generate these values synthetically, based on offline data, etc.
#'
#' @name Bandit
#' @family contextual bandits
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
#'   \item{\code{new()}}{ Generates and initializes a new \code{Bandit} object. }
#'
#'   \item{\code{pre_calculate()}}{
#'      Called right after \code{Simulator} sets its seed,
#'      but before it starts iterating over all time steps \code{t} in T. If you need to initialize random values in a \code{Policy},
#'      this is the place to do so.
#'   }
#'
#'   \item{\code{get_context(t)}}{
#'      Generate or retrieve \code{k}
#'      (the number of \code{Bandit} arms), \code{d} (the number of \code{context} features),
#'      and context vector \code{X} for the current time step \code{t} and return these as
#'      \code{list(k = n_arms, d = n_features, X = context)}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      Return the \code{reward} for a particular \code{action} as previously selected by a \code{Policy}, and,
#'      if known, the reward of the current best arm as \code{list(reward = reward_for_choice_made,
#'      optimal = optimal_reward_value)}.
#'  }
#'
#'   \item{\code{generate_bandit_data()}}{
#'      A helper function that is called before \code{Simulator} starts iterating over all time steps \code{t} in T.
#'      This function is called when \code{bandit$precaching} has been set to \code{TRUE}.
#'      Pregenerate \code{contexts} and \code{rewards} here.
#'   }
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{SyntheticBandit}}
#'
NULL
