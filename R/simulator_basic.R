# make simulator animated and basic... where animated not cache .. hmm
#' @export
SimulatorBasic <- R6::R6Class(
  "SimulatorBasic",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agent_n = NULL,
    horizon = NULL,
    simulations = NULL,
    history = NULL,

    animate = FALSE,
    animate_step = NULL,

    initialize = function(agents,
                          animate = FALSE,
                          animate_step = 1,
                          horizon = 100L,
                          simulations = 100L
                          ) {
      self$horizon <- horizon
      self$simulations <- simulations
      if (!is.list(agents)) agents = list(agents)
      self$agents <- agents
      self$agent_n <- length(agents)
      self$history <- History$new(self$horizon * self$agent_n * self$simulations)

      self$animate <- animate
      self$animate_step <- animate_step
      self$reset()

    },
    reset = function() {
      for (a in 1L:self$agent_n)
        self$agents[[a]]$reset()
        if (self$agents[[a]]$bandit$is_precaching)
          self$agents[[a]]$generate_cache(self$horizon*self$simulations)
    },
    run = function() {
      agent_list <-  matrix(list(), self$agent_n, self$simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent_list[a, s]  <- list(self$agents[[a]]$clone(deep = FALSE))
        }
      }

      counter <- 1L

      for (t in 1L:self$horizon) {
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {

            agent_counter = as.integer(s + ((t - 1L) * self$simulations))

                      agent_list[[a,s]]$bandit_get_context(agent_counter)       # observe the bandit in its context
            action <- agent_list[[a,s]]$policy_get_decision(agent_counter)      # use policy to decide which choice to make (which arm to pick)
            reward <- agent_list[[a,s]]$bandit_get_reward(agent_counter)        # observe the resonse of the bandit in this context
            theta  <- agent_list[[a,s]]$policy_set_reward(agent_counter)        # adjust the policy, update theta

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
