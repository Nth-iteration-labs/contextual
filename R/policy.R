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
#' R6 class \code{Policy} is the parent class of all \code{Policy} implementations in
#' \code{\{contextual\}}. Classes that extend this abstract superclass are expected to take into
#' account the current \code{d} dimensional \code{context},
#' together with a limited set of parameters denoted \code{theta} (summarizing all past actions),
#' to choose one of \code{k} \code{Bandit} arms at each time step \code{t}.
#' On choosing one of the \code{k} arms and receiving its corresponding \code{reward},
#' the \code{Policy} then utilizes the current \code{context}, \code{action} and \code{reward} to
#' update its set of parameters \code{theta}.
#'
#' @name Policy
#' @aliases get_action set_reward set_parameters initialize_theta
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
#'          \code{context$k} (number of arms) and \code{context$d} (number of context feaures)
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
#'          \code{context$k} (number of arms) and \code{context$d} (number of context feaures)
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
#'
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
