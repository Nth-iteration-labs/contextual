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
    precaching = FALSE,
    initialize  = function(k, d, num_users = 1, user_model = NA, seed = 42) {  # carefull with this seed..
      set.seed(seed)
      self$num_users                     <- num_users
      self$user_model                    <- user_model
      self$k                             <- k
      self$d                             <- d
      self$u                             <- d
      if (is.na(self$user_model & num_users > 0)) {
        self$user_model  <- matrix(sample(c(0, 1), replace = TRUE, size = num_users * d), num_users, d )
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
    do_action = function(context, action, t) {
      rewardlist <- list(
        reward     = private$R[action$choice],
        opimal     = private$R[which.max(private$R)]
      )
      rewardlist
    }
  )
)

#' External ContextualBandit
#'
#' ContextualBandit intro
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
#' @importFrom R6 R6Class
#' @name ContextualBandit
#' @examples
#'\dontrun{}
#'
NULL
