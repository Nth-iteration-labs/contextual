#' @export
BasicContextualBandit <- R6::R6Class(
  "BasicContextualBandit",
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
    s = NULL,
    class_name    = "BasicContextualBandit",
    precaching = FALSE,
    # sigma: Standard deviation of the additive noise. Set to zero for no noise.
    initialize  = function(k, common, unique, sigma = 0.1) {
      self$k                                  <- k
      self$d                                  <- common + unique
      self$c                                  <- common
      self$u                                  <- unique
      self$s                                  <- sigma
    },
    post_initialization = function() {
      # Latent parameters that determine expected reward for each arm.
      self$weights                            <- matrix(rnorm(self$d*self$k,mean=0,sd=1),self$d,self$k)
      self$weights                            <- self$weights / norm(self$weights)
      context_common                          <- sample(0:1, self$c*self$k, replace=T,prob=c(0.5,0.5))
      self$context_common                     <- matrix(context_common, self$c, self$k)
    },
    get_context = function(t) {

      context_unique                          <- sample(0:1, self$u, replace=T,prob=c(0.5,0.5))
      self$context_unique                     <- matrix(context_unique, self$u, self$k)

      context_total                           <- rbind2(self$context_unique, self$context_common)

      self$weights                            <- self$weights +
                                                 matrix(rnorm(self$d*self$k,sd=self$s),self$d,self$k)

      dot_rewards                             <- colMeans(t(context_total) %*% self$weights)

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

#' Bandit: BasicContextualBandit
#'
#' BasicContextualBandit intro
#'
#' Following Allen Day DataGenerator
#'
#' @name BasicContextualBandit
#' @family context_commonual subclasses
#'
#' @section Usage:
#' \preformatted{b <- BasicContextualBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{BasicContextualBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new BasicContextualBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core context_commonual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
NULL
