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
      if (is.null(name)) {
        self$name  <- gsub("Policy", "", policy$class_name)
      } else {
        self$name  <- name
      }
      self$reset()
    },
    reset = function() {
      if(!is.null(self$bandit$d) && is.null(self$bandit$unique)) {
        self$bandit$unique <- c(1:self$bandit$d)
      }
      if(!is.null(self$bandit$d) && is.null(self$bandit$shared)) {
        self$bandit$shared <- c(1:self$bandit$d)
      }
      context_initial_params        <- list ()
      context_initial_params$d      <- self$bandit$d
      context_initial_params$k      <- self$bandit$k
      context_initial_params$unique <- self$bandit$unique
      context_initial_params$shared <- self$bandit$shared
      self$policy$set_parameters(context_initial_params)
      self$policy$initialize_theta(context_initial_params$k)
      self$progress_file <- FALSE
      self$log_interval <- 1000L
      agent_t <<- 0L
      policy_t <<- 1L
    },
    do_step = function() {
      agent_t  <<- agent_t + 1L
      context   <- bandit$get_context(agent_t)
      if(!is.null(context$d) && is.null(context$unique)) context$unique <- c(1:context$d)
      if(!is.null(context$d) && is.null(context$shared)) context$shared <- c(1:context$d)
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
#' R6 class \code{Agent} oversees the running of one \code{\link{Bandit}} and \code{\link{Policy}}
#' pair over each time step \code{t}. A \code{\link{Simulator}} instance generally runs multiple
#' \code{Agents} n parallel.
#'
#' @name Agent
#' @aliases do_step get_t set_t
#'
#' @section Usage:
#' \preformatted{
#' agent <- Agent$new(policy, bandit, name=NULL, sparse = 0.0)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'
#'   \item{\code{policy}}{
#'    \code{\link{Policy}} instance.
#'   }
#'  \item{\code{bandit}}{
#'    \code{\link{Bandit}} instance.
#'   }
#'   \item{\code{name}}{
#'    character; sets the name of the \code{Agent}. If \code{NULL} (default), \code{Agent} generates a name
#'    based on its \code{\link{Policy}}'s name.
#'   }
#'   \item{\code{sparse}}{
#'     numeric; artificially reduces the data size by setting a sparsity level for the current
#'     \code{\link{Bandit}} and \code{\link{Policy}} pair.
#'     When set to a value between \code{0.0} (default) and \code{1.0} only a fraction \code{sparse} of
#'     the \code{\link{Bandit}}'s data is randomly chosen to be available to improve the \code{Agent}'s
#'     \code{\link{Policy}} through \code{policy$set_reward}.
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new()}}{ generates and instantializes a new \code{Agent} instance. }
#'
#'   \item{\code{do_step()}}{
#'      completes one time step by consecutively calling \code{bandit$get_context()},
#'      \code{policy$get_action()}, \code{bandit$get_reward()} and \code{policy$set_reward()}.
#'    }
#'
#'   \item{\code{set_t(t)}}{
#'      integer; Sets the current time step to \code{t}.
#'   }
#'
#'   \item{\code{get_t()}}{
#'      returns the current time step \code{t}.
#'   }
#'
#'   }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{MabWeightBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \donttest{
#'   policy    <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'   bandit    <- MabWeightBandit$new(weights = c(0.6, 0.1, 0.1))
#'
#'   agent     <- Agent$new(policy, bandit, name = "E.G.", sparse = 0.5)
#'
#'   history   <- Simulator$new(agents = agent,
#'                              horizon = 10,
#'                              simulations = 10,
#'                              progress_file = TRUE)$run()
#' }
NULL
