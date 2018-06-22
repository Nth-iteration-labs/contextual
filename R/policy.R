#' @export
Policy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  public = list(
    action        = NULL,
    theta         = NULL,
    theta_to_arms = NULL,
    k             = NULL,
    d             = NULL,
    cl            = NULL,
    class_name    = "Policy",
    initialize = function() {
      self$theta  <- list()
      self$action <- list()
    },
    get_action = function(t, context) {
      # Selects an arm based on self$theta and context, returns it in action$choice.
      stop("Policy$get_action() has not been implemented.", call. = FALSE)
    },
    set_reward = function(t, context, action, reward) {
      # Updates parameters in theta based on reward awarded by bandit.
      stop("Policy$set_reward() has not been implemented.", call. = FALSE)
    },
    set_parameters = function() {
      # Policy parameter (not theta!) initialisation happens here.
      stop("Policy$set_parameters() has not been implemented.", call. = FALSE)
    },
    initialize_theta = function() {
      # Called during contextual's initialisation.
      # Copies theta_to_arms k times, makes the copies available through theta.
      if (!is.null(self$theta_to_arms)) {
        for (param_index in seq_along(self$theta_to_arms)) {
          self$theta[[ names(self$theta_to_arms)[param_index] ]] <- rep(list(self$theta_to_arms[[param_index]]),self$k)
        }
      }
    }
  )
)



#' Policy
#'
#' The R6 class \code{Policy} is the parent class of all \code{Policy} implementations in
#' \code{\{contextual\}}. Classes that extend this abstract \code{Policy}
#' superclass are expected to take into account the current \code{d} dimensional \code{context},
#' together with a limited set of parameters denoted \code{theta} (summarizing all past actions),
#' to choose one of a \code{Bandit}'s arms at each time step \code{t}.
#' On choosing one of the {k} arms of the Bandit and receiving its corresponding \code{reward},
#' the \code{Policy} then uses the current \code{context}, \code{action} and \code{reward} to
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
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{name}}{
#'    A character string identifying this \code{Policy} It is, amongst others, saved to the \code{History} log
#'    and displayed in summaries and plots.
#'   }
#' }
#'
#' @section Methods:
#'
#' \describe{
#'   \item{\code{new(name)}}{
#'     Generates and initializes a new \code{Policy} object.
#'   }
#'
#'   \item{\code{initialize(name)}}{
#'     #TODO
#'   }
#'
#'   \item{\code{set_parameters()}}{
#'    This helper function, called during a Policy's initialisation, assigns the values
#'    it finds in list \code{self$theta_to_arms} to each of the Policy's k arms.
#'    The parameters defined here can then be accessed by arm index in the following way:
#'    \code{theta[[index_of_arm]]$parameter_name}.
#'   }
#'
#'   \item{\code{get_action(t, context)}}{
#'    Calculates which arm to play based on the current values in named list \code{theta}
#'    and the current \code{context}. Returns a named list
#'    \code{list(choice = arm_chosen_by_policy)} that holds the index of the arm
#'    to play.
#'   }
#'
#'   \item{\code{set_reward(t, context, action, reward)}}{
#'    Returns the named list
#'    \code{list(reward = reward_for_choice_made, optimal = optimal_reward_value)} containing the \code{reward}
#'    for the \code{action} previously returned by \code{policy} and, optionally, the \code{optimal} reward
#'    at the current time \code{t}.
#'    }
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{ContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
