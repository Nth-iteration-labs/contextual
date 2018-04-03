#' @export
Bandit <- R6::R6Class(
  "Bandit",
  portable = TRUE,
  class    = FALSE,
  public   = list(
    k             = NULL,  # Number of arms (integer)
    d             = NULL,  # Dimension of context feature vector (integer)
    precaching    = FALSE, # Pregenerate context & reward matrices? (boolean)
    get_context = function(t) {
      stop("Bandit subclass needs to implement bandit$get_context()", call. = FALSE)
      # Return a list with self$k, self$d and, where applicable, a context vector X.
      list(k = n_arms, d = n_features, X = context)
    },
    do_action = function(action, t) {
      stop("Bandit subclass needs to implement bandit$do_action()", call. = FALSE)
      # Return a list with the reward and, if known, the reward of the best arm.
      list(reward = reward_for_choice_made, optimal = optimal_reward)
    },
    generate_bandit_data = function(n) {
      # Called when precaching is TRUE. Pregenerate contexts and rewards here.
      stop("Bandit subclass needs to implement bandit$generate_cache()
           when bandit$precaching is TRUE.", call. = FALSE)
    }
  )
)

#' External Bandit
#'
#' Bandit intro
#'
#' @section Usage:
#' \preformatted{b <- Bandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Bandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Bandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @family contextual classes
#' @name Bandit
#' @examples
#'\dontrun{}
#'
NULL
