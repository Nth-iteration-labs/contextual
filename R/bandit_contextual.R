#' @export
ContextualBandit <- R6::R6Class(
  "ContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    R = NULL
  ),
  public = list(
    shared_context = NULL,
    n_disjoint = NULL,
    n_shared = NULL,
    class_name    = "ContextualBandit",
    precaching = FALSE,
    initialize  = function(k, n_disjoint = 5, n_shared = 6) {

      self$k                                <- k
      self$d                                <- n_disjoint  + n_shared
      self$n_disjoint                       <- n_disjoint
      self$n_shared                         <- n_shared
      self$d_disjoint                       <- c(1:n_disjoint)
      self$d_shared                         <- c((n_disjoint+1):(self$d))
    },
    post_initialization = function() {
      self$shared_context                   <- matrix(runif(self$n_shared *self$k), self$n_shared, self$k)
    },
    get_context = function(t) {

      user_features                         <- runif(self$n_disjoint )
      disjoint_context                      <- matrix(user_features, self$n_disjoint , self$k )

      X                                     <- rbind2(disjoint_context,self$shared_context)

      rewards                               <- jitter(colMeans(X),amount=0)
      private$R                             <- rep(0,self$k)
      private$R[which.max(rewards)]         <- 1

      contextlist                           <- list(
        k = self$k,
        d = self$d,
        d_disjoint = self$d_disjoint,
        d_shared = self$d_shared,
        X = X
      )
      contextlist
    },
    get_reward = function(t, context, action) {
      rewardlist <- list(
        reward                   = private$R[action$choice],
        optimal_reward_value     = private$R[which.max(private$R)]
      )
      rewardlist
    }
  )
)

#' Bandit: ContextualBandit
#'
#' ContextualBandit intro
#'
#' @name ContextualBandit
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{b <- ContextualBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{ContextualBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new ContextualBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
NULL
