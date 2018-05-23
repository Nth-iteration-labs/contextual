#' @export
LifPolicy <- R6::R6Class(
  "LifPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    first = NULL,

    inttime = NULL   ,                  # Integration time
    amplitude = NULL,                   # Amplitude
    learnrate = NULL,                   # Learnrate
    omega = NULL,                       # Omega
    x0_start = NULL,                    # x0 start value

    initialize = function(inttime,amplitude,learnrate,omega,x0_start, name = "LockInFeedback") {
      super$initialize(name)
      self$inttime   <- inttime
      self$amplitude <- amplitude
      self$learnrate <- learnrate
      self$omega     <- omega
      self$x0_start  <- x0_start
    },
    set_parameters = function() {
      self$theta_to_arms <- list('x0' = x0_start, 'Y' = rep(NA, inttime))
    },
    get_action = function(t, context) {
      action$choice <- self$theta$x0[[1]] + amplitude*cos(omega * t)
      action
    },
    set_reward = function(t, context, action, reward) {
      reward   <- reward$reward
      y <- amplitude*cos(omega * t)*reward
      self$theta$Y[[1]] <- c(y, self$theta$Y[[1]])[seq_along(self$theta$Y[[1]])]
      if (t > inttime)
        self$theta$x0[[1]] <- self$theta$x0[[1]] + learnrate * sum( self$theta$Y[[1]] ) / inttime
      self$theta
    }
  )
)

#' Policy: LiF
#'
#' LifPolicy intro

#' @name LifPolicy
#' @family contextual subclasses
#'
#' @section Usage:
#' \preformatted{b <- LifPolicy$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{LifPolicy} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new LifPolicy, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
NULL

