#' @export
SimulatorBasic <- R6::R6Class(
  "SimulatorBasic",
  portable = FALSE,
  inherit = Contextual,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agent_n = NULL,
    animate = FALSE,
    animate_step = 2,
    horizon = 100L,
    simulations = 1L,
    history = list(),
    initialize = function(agents,
                          animate = FALSE,
                          animate_step = 1) {
      if (!is.list(agents)) agents = list(agents)
      self$history <- History$new()
      self$agents <- agents
      self$agent_n <- length(agents)
      self$animate <- animate
      self$animate_step <- animate_step
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n)
        self$agents[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {

      self$horizon <- horizon
      self$simulations <- simulations

      agent_list <-  matrix(list(), self$agent_n, simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent_list[a, s]  <- list(self$agents[[a]]$clone(deep = FALSE))
        }
      }
      counter <- 1L
      n <- self$horizon * self$agent_n * self$simulations
      self$history$reset(n)

      for (t in 1L:self$horizon) {
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {

                      agent_list[[a,s]]$bandit_get_context(t)                   # observe the bandit in its context
            action <- agent_list[[a,s]]$policy_get_decision(t)                  # use policy to decide which choice to make (which arm to pick)
            reward <- agent_list[[a,s]]$bandit_get_reward(t)                    # observe the resonse of the bandit in this context
            theta  <- agent_list[[a,s]]$policy_set_reward(t)                    # adjust the policy, update theta

            self$history$save_agent(counter,                                    # save the results to the history log
                                    t,
                                    action,
                                    reward,
                                    agent_list[[a,s]]$policy$name,
                                    s,
                                    theta)

            counter <- counter + 1L
          }
        }
        if (self$animate == TRUE && t %% self$animate_step == 0) {              # animate while computing?
          plot$grid(self$history$get_data_table()[t != 0L])
        }
      }
      self$history$get_data_table()
    }
  )
)

#' External SimulatorBasic
#'
#' SimulatorBasic intro
#'
#' @section Usage:
#' \preformatted{b <- SimulatorBasic$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{SimulatorBasic} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new SimulatorBasic, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name SimulatorBasic
#' @examples
#'\dontrun{}
#'
NULL
