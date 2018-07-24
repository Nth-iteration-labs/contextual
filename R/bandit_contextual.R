#' @export
ContextualBandit <- R6::R6Class(
  "ContextualBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    R = NULL,
    X = NULL
  ),
  public = list(
    user_model    = NULL,
    num_users     = NULL,
    u             = NULL,
    class_name    = "ContextualBandit",
    precaching = FALSE,
    initialize  = function(k, d, num_users = 1, user_model = NA) {
      self$num_users                     <- num_users
      self$user_model                    <- user_model
      self$k                             <- k
      self$d                             <- d
    },
    post_initialization = function() {
      if (is.na(self$user_model & self$num_users > 0)) {
        self$user_model  <- matrix(sample(c(0, 1), replace = TRUE, size = self$num_users * self$d), self$num_users, self$d )
      }
    },
    get_context = function(t) {
      private$X                          <- matrix(runif(self$d*self$k),self$d,self$k)
      user_features                      <- NA
      if (self$num_users > 0) {
        user                             <- sample(self$num_users,1,replace = TRUE)
        user_features                    <- self$user_model[user,]
        weights                          <- user_features %*% private$X
      } else {
        weights                          <- colSums(private$X)/self$d
      }

      private$R                          <- rep(0,self$k)
      private$R[which.max(weights)]      <- 1
      contextlist <- list(
        k = self$k,
        d = self$d,
        X = private$X,
        U = user_features
      )
      contextlist
    },
    get_reward = function(t, context, action) {
      rewardlist <- list(
        reward     = private$R[action$choice],
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
