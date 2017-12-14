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
    horizon = 100L,
    simulations = 1L,
    history = NULL,

    initialize = function(agents) {
      if (!is.list(agents)) agents = list(agents)
      self$history <- History$new()
      self$agents <- agents
      self$agent_n <- length(agents)
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

      agent <-  matrix(list(), self$agent_n, simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a, s]  <- list(self$agents[[a]]$clone(deep = FALSE))

        }
      }

      workers <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(workers)
      doParallel::registerDoParallel(cl)

      n <- self$horizon  * self$agent_n
      self$history$reset(n)

      `%do%` <- foreach::`%do%`
      `%dorng%` <- doRNG::`%dorng%`
      `%dopar%` <- foreach::`%dopar%`

      parallel_results <- foreach::foreach(
        s = 1L:self$simulations,
        .inorder = FALSE,
        .packages = c("data.table")
      ) %dorng% {
        counter <- 1L
        for (a in 1L:self$agent_n) {
          for (t in 1L:self$horizon) {

            agent[[a,s]]$bandit_get_context(t)                                  # observe the bandit in its context
            action <- agent[[a,s]]$policy_get_decision(t)                       # use policy to decide which choice to make (which arm to pick)
            reward <- agent[[a,s]]$bandit_get_reward(t)                         # observe the resonse of the bandit in this context
            agent[[a,s]]$policy_set_reward(t)                                   # adjust the policy, update theta

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
