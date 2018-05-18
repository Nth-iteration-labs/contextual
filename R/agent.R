#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  class = FALSE,
  private = list(
    t = NULL
  ),
  public = list(
    policy = NULL,
    bandit = NULL,
    sim_index = NULL,
    agent_index = NULL,
    initialize = function(policy, bandit) {
      self$bandit                 <- bandit
      self$policy                 <- policy
      self$policy$k               <- self$bandit$k
      self$policy$d               <- self$bandit$d
      self$reset()
    },
    reset = function() {
      self$policy$set_parameters()
      self$policy$initialize_theta()
      private$t <- 0
    },
    set_t = function(t) {
      private$state$t <- t
    },
    get_t = function(t) {
      t
    },
    do_step = function() {
      private$t <- private$t + 1
      context = bandit$get_context(private$t)
      action  = policy$get_action(private$t, context)
      reward  = bandit$get_reward(private$t, context, action)
      theta = policy$set_reward(private$t, context, action, reward)
      list(context = context, action = action,reward = reward,theta = theta)
    }
  )
)

#' Agent
#'
#' The R6 class \code{Agent} is responsible for the state, flow of information
#' between and the running of one \code{Bandit} and \code{Policy} pair.
#' As such, multiple \code{Agent}s can be run in parallel with each separate Agent keeping
#' track of \code{t} and the parameters in named list \code{theta} for its assigned \code{Policy}
#' and \code{Bandit} pair.
#'
#' @name Agent
#' @family contextual
#'
#' @section Usage:
#' \preformatted{
#' agent <- Agent$new(policy, bandit)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{policy}}{
#'    A \code{Policy} object, expected to take into account the current \code{d} dimensional \code{context}
#'    feature vector \code{X}, together with a limited set of parameters denoted \code{theta} (summarizing
#'    all past actions), to choose one of the k arms of its corresponding bandit's arms at each time step \code{t}.
#'   }
#'  \item{\code{bandit}}{
#'    A \code{Bandit} object, responsible for both the generation of \code{d} dimensional \code{context}
#'    vectors \code{X} and the \code{k} I.I.D. distributions each generating a \code{reward} for each of
#'    its \code{k} arms at each time step \code{t}.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{set_t(t)}}{
#'      Setter function, sets the state of the state variable holding the current time step \code{t}.
#'   }
#'
#'   \item{\code{do_step()}}{
#'      Completes one time step \code{t} by consecutively calling
#'      \code{bandit$get_context()}, \code{policy$get_action()}, \code{bandit$get_reward()} and \code{policy$set_reward()}.
#'    }
#'
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit classes: \code{\link{Bandit}}, \code{\link{BasicBandit}},
#' \code{\link{LiSamplingOfflineBandit}}, \code{\link{SyntheticBandit}}
#'
NULL
