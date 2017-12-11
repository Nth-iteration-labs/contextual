#' @export
SimulatorBasic <- R6::R6Class(
  "SimulatorBasic",
  portable = FALSE,
  inherit = Contextual,
  private = list(rewards = NULL),
  public = list(
    agent_list = NULL,
    agent_n = NULL,
    animate = FALSE,
    animate_step = 2,
    horizon = 100L,
    simulations = 1L,
    history = list(),
    initialize = function(agent_list,
                          animate = FALSE,
                          animate_step = 1) {
      self$history <- History$new()
      self$agent_list <- agent_list
      self$agent_n <- length(agent_list)
      self$animate <- animate
      self$animate_step <- animate_step
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n)
        self$agent_list[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {

      self$horizon <- horizon
      self$simulations <- simulations

      agent <-  matrix(list(), self$agent_n, simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a, s]  <- list(self$agent_list[[a]]$clone(deep = FALSE))        ## deep may be very important when bandits or policies
                                                                                ## change per type?
                                                                                ## but *much* slower, and leads to circular reference
                                                                                ## .. file bug report at R6?
        }
      }
      counter <- 1L
      n <- self$horizon * self$agent_n * self$simulations
      self$history$reset(n)

      for (t in 1L:self$horizon) {
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {

                      agent[[a,s]]$bandit_get_context(t)                        # observe the bandit in its context
            action <- agent[[a,s]]$policy_get_decision(t)                       # use policy to decide which choice to make (which arm to pick)
            reward <- agent[[a,s]]$bandit_get_reward(t)                         # observe the resonse of the bandit in this context
            theta  <- agent[[a,s]]$policy_set_reward(t)                         # adjust the policy, update theta

            self$history$save_agent(counter,                                    # save the results to the history log
                                    t,
                                    action,
                                    reward,
                                    agent[[a,s]]$policy$name,
                                    s,
                                    theta)

            counter <- counter + 1L
          }
        }
        if (self$animate == TRUE && t %% self$animate_step == 0) {              # animate while computing?
          plot$plot_grid(self$history$get_data_table()[t != 0L])
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
