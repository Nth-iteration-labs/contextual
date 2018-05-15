#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  class = FALSE,
  private = list(
    state = NULL,
    step  = NULL
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
      private$state$t             <- 0
      private$state$theta         <- self$policy$initialize_theta()
      private$step$context        <- matrix()
      private$step$action         <- list()
      private$step$reward         <- list()
    },
    set_t = function(t) {
      private$state$t <- t
    },
    do_step = function() {
      private$state$t <- private$state$t + 1
      list(context = bandit_get_context(),
           action  = policy_get_action(),
           reward  = bandit_get_reward(),
           theta   = policy_set_reward())
    },
    bandit_get_context = function() {
      private$step$context <- bandit$get_context(private$state$t)
      private$step$context
    },
    policy_get_action = function() {
      policy$set_theta(private$state$theta)
      private$step$action <- policy$get_action(private$state$t, private$step$context)
      private$step$action
    },
    bandit_get_reward = function() {
      private$step$reward <- bandit$get_reward(private$state$t,private$step$context, private$step$action)
      private$step$reward
    },
    policy_set_reward = function() {
      if (!is.null(private$step$reward)) {
        private$state$theta <- policy$set_reward(private$state$t, private$step$context, private$step$action, private$step$reward)
        private$state$theta
      }
    }
  )
)

#' Agent
#'
#' \code{Agent}: The R6 class \code{Agent} is responsible for the state, flow of information
#' between and the running of one \code{Bandit}/\code{Policy} pair.
#' As such, multiple \code{Agent}s can be run in parallel with each separate Agent keeping
#' track of \code{t} and the parameters in \code{theta} for its assigned \code{Policy} and \code{Bandit} pair.
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
#'      Setter function, sets the state of the current time step variable \code{t}.
#'   }
#'
#'   \item{\code{do_step()}}{
#'       This convenience function completes one time step \code{t} by consecutively calling
#'       bandit_get_context(), policy_get_action(), bandit_get_reward() and policy_set_reward().
#'    }
#'
#'   \item{\code{bandit_get_context()}}{
#'      Calls \code{bandit$get_context(t)}, which returns the current \code{d} dimensional \code{context}
#'      feature vector \code{X} in a list together with the number of arms and context features as
#'      \code{list(k = n_arms, d = n_features, X = context)}.
#'    }
#'
#'   \item{\code{policy_get_action()}}{
#'      Calls \code{policy$get_action(t, X)}, which chooses an arm to play based on the current values
#'      of its parameters \code{theta} and the current \code{context}.
#'      Returns an \code{action} list that contains the index of the suggested arm as \code{action$choice}.
#'
#'    }
#'   \item{\code{bandit_get_reward()}}{

#'    }
#'
#'   \item{\code{policy_set_reward()}}{
#'
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
