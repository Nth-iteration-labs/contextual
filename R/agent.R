#' @export
Agent <- R6::R6Class(
  "Agent",
  portable = FALSE,
  class = FALSE,
  public = list(
    policy = NULL,
    bandit = NULL,
    sim_index = NULL,
    agent_index = NULL,
    name = NULL,
    agent_t = NULL,
    policy_t = NULL,
    progress_file = NULL,
    log_interval = NULL,
    sparse = NULL,
    initialize = function(policy, bandit, name=NULL, sparse = 0.0) {

      self$bandit                 <- bandit
      self$policy                 <- policy
      self$sparse                 <- sparse
      self$policy$k               <- self$bandit$k
      self$policy$d               <- self$bandit$d
      self$policy$d_disjoint      <- self$bandit$d_disjoint
      self$policy$d_shared        <- self$bandit$d_shared

      if (is.null(name)) {
        self$name  <- gsub("Policy", "", policy$class_name)
      } else {
        self$name  <- name
      }

      self$reset()
    },
    reset = function() {
      self$policy$set_parameters()
      self$policy$initialize_theta()
      self$progress_file <- FALSE
      self$log_interval <- 1000L
      agent_t <<- 0L
      policy_t <<- 1L
    },
    do_step = function() {
      agent_t  <<- agent_t + 1L
      context   <- bandit$get_context(agent_t)
      action    <- policy$get_action (policy_t, context)
      reward    <- bandit$get_reward (agent_t, context, action)
      if (is.null(reward)) {
        theta   <- NULL
      } else {
        if (self$sparse == 0.0 || runif(1) > self$sparse) {
          theta   <- policy$set_reward (policy_t, context, action, reward)
        } else {
          theta   <- policy$theta
        }
        policy_t  <<- policy_t + 1L
      }

      if(isTRUE(self$progress_file)) {
        if (agent_t %% self$log_interval == 0) {
          cat(paste0("[",format(Sys.time(), format = "%H:%M:%OS6"),"] ",sprintf("%9s", agent_t)," > step - ",
                     sprintf("%-20s", self$name)," running ",bandit$class_name,
                     " and ",policy$class_name,"\n"),file = "progress.log", append = TRUE)
        }

      }
      list(context = context, action = action, reward = reward, theta = theta)
    },
    set_t = function(t) {
      agent_t <<- t
    },
    get_t = function(t) {
      agent_t
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
#' @aliases do_step get_t set_t
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
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{ContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
