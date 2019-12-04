#' @export
ContinuumBanditUnimodal <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    arm_function = NULL,
    c1 = NULL,
    c2 = NULL,
    class_name = "ContinuumBanditUnimodal",
    initialize   = function() {
      self$c2 <- 1
      self$arm_function <- function(x, c1 = 0.25, c2 = 0.75) {
        -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
      }
      super$initialize()
      self$d            <- 1
      self$k            <- 1
    },
    post_initialization = function(){
      self$c1 <- runif(1,0.25,0.75)
    },
    get_context = function(t) {
      context           <- list()
      context$k         <- self$k
      context$d         <- self$d
      context
    },
    get_reward = function(t, context, action) {
      reward  <- list(
        reward                   = self$arm_function(action$choice, self$c1, self$c2),
        optimal_reward           = self$c2
      )
    }
  )
)

#' Bandit: ContinuumBandit
#'
#' A function based continuum multi-armed bandit
#' where arms are chosen from a subset of the real line and the mean rewards
#' are assumed to be a continuous function of the arms.
#'
#' @section Usage:
#' \preformatted{
#'    bandit <- ContinuumBandit$new(FUN)
#' }
#'
#' @name ContinuumBandit
#'
#'
#' @section Arguments:
#' \describe{
#'   \item{FUN}{continuous function.}
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(FUN)}}{ generates and instantializes a new \code{ContinuumBandit} instance. }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the current \code{d x k} dimensional matrix \code{context$X},
#'      the number of arms \code{context$k} and the number of features \code{context$d}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward} and, where computable,
#'         \code{reward$optimal} (used by "oracle" policies and to calculate regret).
#'  }
#
#' }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' horizon            <- 1500
#' simulations        <- 100
#'
#' continuous_arms  <- function(x) {
#'   -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4)
#' }
#'
#' int_time    <- 100
#' amplitude   <- 0.2
#' learn_rate  <- 0.3
#' omega       <- 2*pi/int_time
#' x0_start    <- 2.0
#'
#' policy             <- LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start)
#'
#' bandit             <- ContinuumBandit$new(FUN = continuous_arms)
#'
#' agent              <- Agent$new(policy,bandit)
#'
#' history            <- Simulator$new(     agents = agent,
#'                                          horizon = horizon,
#'                                          simulations = simulations,
#'                                          save_theta = TRUE             )$run()
#'
#' plot(history, type = "average", regret = FALSE)
#' }
NULL
