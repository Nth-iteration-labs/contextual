#' @export
Policy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  public = list(
    action        = NULL,      # action results (list)
    theta         = NULL,      # policy parameters theta (list)
    theta_to_arms = NULL,      # theta to arms "helper" (list)
    is_oracle     = NULL,      # is policy an oracle? (logical)
    class_name    = "Policy",  # policy name - required (character)
    initialize = function() {
      self$theta  <- list()    # initializes theta list
      self$action <- list()    # initializes action list
      is_oracle   <- FALSE     # very seldom TRUE
      invisible(self)
    },
    get_action = function(t, context) {
      # Selects an arm based on self$theta and context, returns it in action$choice.
      stop("Policy$get_action() has not been implemented.", call. = FALSE)
    },
    set_reward = function(t, context, action, reward) {
      # Updates parameters in theta based on reward awarded by bandit.
      stop("Policy$set_reward() has not been implemented.", call. = FALSE)
    },
    set_parameters = function(context_params) {
      # Policy parameter (not theta!) initialisation happens here.
      stop("Policy$set_parameters() has not been implemented.", call. = FALSE)
    },
    initialize_theta = function(k) {
      # Called during contextual's initialisation.
      # Copies theta_to_arms k times, makes the copies available through theta.
      if (!is.null(self$theta_to_arms)) {
        for (param_index in seq_along(self$theta_to_arms)) {
          self$theta[[ names(self$theta_to_arms)[param_index] ]] <- rep(list(self$theta_to_arms[[param_index]]),k)
        }
      }
      self$theta
    }
  )
)

#' Policy
#'
#' Parent or superclass of all \code{\{contextual\}} \code{Policy} subclasses.
#'
#' On every \emph{t} = \{1, \ldots, T\}, a policy receives \code{d} dimensional feature vector or
#' \code{d x k} dimensional matrix
#' \code{context$X}*, the current number of \code{\link{Bandit}} arms in \code{context$k}, and the current
#' number of contextual features in \code{context$d}.
#'
#' To make sure a policy supports both contextual feature vectors and matrices in \code{context$X}, it is
#' suggested any contextual policy makes use of \pkg{contextual}'s \code{get_arm_context(context$X, arm)}
#' utility function to obtain the current context for a particular arm, \code{and get_full_context(X, d, k)}
#' where a policy needs to obtain of the full \code{d x k} matrix.
#'
#' It has to compute which of the \code{k}
#' \code{\link{Bandit}} arms to pull by taking into account this contextual information plus the policy's
#' current parameter values stored in the named list \code{theta}. On selecting an arm, the policy then
#' returns its index as \code{action$choice}.
#'
#' ![](3bpolicy.jpeg "contextual diagram: get context")
#'
#' On pulling a \code{\link{Bandit}} arm the policy receives a \code{\link{Bandit}} reward through
#' \code{reward$reward}. In combination with the current \code{context$X}* and \code{action$choice},
#' this reward can then be used to update to the policy's parameters as stored in list \code{theta}.
#'
#' ![](3dpolicy.jpeg "contextual diagram: get context")
#'
#' &ast; Note: in context-free scenario's, \code{context$X} can be omitted.
#'
#' @name Policy
#' @aliases get_action set_reward set_parameters initialize_theta policy theta
#'
#' @section Usage:
#' \preformatted{
#'   policy <- Policy$new()
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new()}}{
#'     Generates and initializes a new \code{Policy} object.
#'   }
#'
#'   \item{\code{get_action(t, context)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'      }
#'      computes which arm to play based on the current values in named list \code{theta}
#'      and the current \code{context}. Returns a named list containing
#'      \code{action$choice}, which holds the index of the arm to play.
#'   }
#'
#'   \item{\code{set_reward(t, context, action, reward)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'          \item \code{reward}:  list, containing \code{reward$reward} and, if available,
#'          \code{reward$optimal} (as set by \code{bandit}).
#'      }
#'    utilizes the above arguments to update and return the set of parameters in list \code{theta}.
#'    }
#'
#'   \item{\code{set_parameters()}}{
#'    Helper function, called during a Policy's initialisation, assigns the values
#'    it finds in list \code{self$theta_to_arms} to each of the Policy's k arms.
#'    The parameters defined here can then be accessed by arm index in the following way:
#'    \code{theta[[index_of_arm]]$parameter_name}.
#'   }
#'
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
NULL
