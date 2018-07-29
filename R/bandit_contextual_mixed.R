#' @export
MixedContextualBandit <- R6::R6Class(
  "MixedContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    weights = NULL,
    rewards = NULL,
    context_common = NULL,
    context_unique = NULL,
    c = NULL,
    u = NULL,
    jitter = NULL,
    class_name    = "MixedContextualBandit",
    precaching = FALSE,
    initialize  = function(k, common, unique, classes) {
      self$k                                  <- k
      self$c                                  <- common
      self$u                                  <- unique
      self$d                                  <- common + unique
      self$cl                                 <- classes
    },
    post_initialization = function() {
      self$weights                            <- matrix(runif(self$d, -1, 1),self$d,self$k)
      self$weights                            <- self$weights / norm(self$weights, type = "F")

      self$context_unique                     <- matrix(rnorm(self$u), self$u, self$k)

      self$context_common                     <- matrix(rnorm(self$c*self$k), self$c, self$k)
    },
    get_context = function(t) {



      context_total                           <- rbind2(self$context_unique, self$context_common)

      dot_rewards                             <- colMeans(context_total * self$weights)

      self$rewards                            <- rep(0,self$k)
      self$rewards[which.max(dot_rewards)]    <- 1

      context_commonlist                      <- list(
        k = self$k,
        d = self$d,
        d_disjoint = self$d_disjoint,
        X = context_total
      )
      context_commonlist
    },
    get_reward = function(t, context_common, action) {
      rewardlist <- list(
        reward                   = self$rewards[action$choice],
        optimal_reward_value     = self$rewards[which.max(self$rewards)]
      )
      rewardlist
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
