#' @export
ContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    arm_function = NULL,
    class_name = "ContinuumBandit",
    initialize   = function(FUN) {
      self$arm_function <- FUN
      super$initialize()
      self$d            <- 1
      self$k            <- 1
    },
    get_context = function(t) {
      context           <- list()
      context$k         <- self$k
      context$d         <- self$d
      context
    },
    get_reward = function(t, context, action) {
      reward            <- list()
      reward$reward     <- self$arm_function(action$choice)
      reward
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
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
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
