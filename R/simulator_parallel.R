#' @import foreach
#' @import doParallel
#' @import doRNG
#' @export
SimulatorParallel <- R6::R6Class(
  "SimulatorParallel",
  portable = FALSE,
  class = FALSE,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agent_n = NULL,
    horizon = NULL,
    simulations = NULL,
    history = NULL,

    initialize = function(agents,horizon = 100L, simulations = 100L) {
      self$horizon <- horizon
      self$simulations <- simulations
      if (!is.list(agents)) agents = list(agents)
      self$agents <- agents
      self$agent_n <- length(agents)
      self$history <- History$new(self$horizon * self$agent_n * self$simulations)
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n) {
        self$agents[[a]]$reset()
        if (self$agents[[a]]$bandit$is_precaching)
          self$agents[[a]]$generate_cache(self$horizon*self$simulations)
      }
    },
    run = function() {
      agent <-  matrix(list(), self$agent_n, self$simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a, s]  <- list(self$agents[[a]]$clone(deep = FALSE))

        }
      }

      workers <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(workers)
      doParallel::registerDoParallel(cl)

      `%do%` <- foreach::`%do%`
      `%dorng%` <- doRNG::`%dorng%`
      `%dopar%` <- foreach::`%dopar%`

      sims = self$simulations

      parallel_results <- foreach::foreach(
        s = 1L:self$simulations,
        .inorder = FALSE,
        .export = c("self"),
        .packages = c("data.table")
      ) %dopar% {
        counter <- 1L
        for (a in 1L:self$agent_n) {
          for (t in 1L:self$horizon) {

            agent_counter = as.integer(s + ((t - 1L) * self$simulations))

            agent[[a,s]]$bandit_get_context(agent_counter)                      # observe the bandit in its context
            action <- agent[[a,s]]$policy_get_decision(agent_counter)           # use policy to decide which choice to make (which arm to pick)
            reward <- agent[[a,s]]$bandit_get_reward(agent_counter)             # observe the resonse of the bandit in this context
            agent[[a,s]]$policy_set_reward(agent_counter)                       # adjust the policy, update theta

            self$history$save_agent(counter,                                    # save the results to the history log
                                    t,
                                    action,
                                    reward,
                                    agent[[a,s]]$policy$name,
                                    s)

            counter <- counter + 1L
          }
        }
        dth <- self$history$get_data_table()
        dth[sim != 0]
      }
      parallel_results <- data.table::rbindlist(parallel_results)



      self$history$set_data_table(parallel_results)

      parallel::stopCluster(cl)

      parallel_results
    }
  )
)

#' External SimulatorParallel
#'
#' SimulatorParallel intro
#'
#' @section Usage:
#' \preformatted{b <- SimulatorParallel$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{SimulatorParallel} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new SimulatorParallel, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name SimulatorParallel
#' @examples
#'\dontrun{}
#'
NULL
