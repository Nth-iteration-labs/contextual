#' @export
MixedContextualBandit <- R6::R6Class(
  "MixedContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    weights = NULL,
    rewards = NULL,
    s = NULL,
    u = NULL,
    context_shared = NULL,
    context_unique = NULL,
    jitter = NULL,
    class_name    = "MixedContextualBandit",
    precaching = FALSE,
    initialize  = function(k, shared, unique) {
      self$k                                  <- k
      self$s                                  <- shared
      self$u                                  <- unique
      self$d                                  <- shared + unique
    },
    post_initialization = function() {
      self$weights                            <- matrix(runif(self$d, -1, 1),self$d,self$k)
      self$weights                            <- self$weights / norm(self$weights, type = "F")


      self$context_shared                     <- matrix(rnorm(self$s*self$k), self$s, self$k)
    },
    get_context = function(t) {

      self$context_unique                     <- matrix(rnorm(self$u), self$u, self$k)

      context_total                           <- rbind2(self$context_unique, self$context_shared)

      dot_rewards                             <- colMeans(context_total * self$weights)

      self$rewards                            <- rep(0,self$k)
      self$rewards[which.max(dot_rewards)]    <- 1

      context_list                            <- list(
        k = self$k,
        d = self$d,
        X = context_total
      )
    },
    get_reward = function(t, context_common, action) {
      rewardlist <- list(
        reward                   = self$rewards[action$choice],
        optimal_reward_value     = self$rewards[which.max(self$rewards)]
      )
    }
  )
)

#' Bandit: MixedContextualBandit
#'
#' MixedContextualBandit intro
#'
#' Following Allen Day DataGenerator
#'
#' @name MixedContextualBandit
#' @family context_commonual subclasses
#'
#' @section Usage:
#' \preformatted{b <- MixedContextualBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{MixedContextualBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new MixedContextualBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core context_commonual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
NULL
