#' @import foreach
#' @import doParallel
#' @import doRNG
#' @export
SimulatorParallel <- R6::R6Class(
  "SimulatorParallel",
  private = list(rewards = NULL),
  public = list(
    agent_list = NULL,
    agent_n = NULL,
    horizon = 100L,
    simulations = 1L,
    history = NULL,

    initialize = function(agent_list) {
      self$history = History$new()
      self$agent_list = agent_list
      self$agent_n = length(agent_list)
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n)
        self$agent_list[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {

      self$horizon = horizon
      self$simulations = simulations
      agent =  matrix(list(), self$agent_n, simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a, s]  = list(self$agent_list[[a]]$clone(deep = TRUE))
        }
      }

      workers <- parallel::detectCores() - 1
      cl <- parallel::makeCluster(workers)
      doParallel::registerDoParallel(cl)
      n = as.integer(ceiling(self$horizon / workers *
                               self$agent_n *
                               self$simulations))
      self$history$reset(n)
      `%dorng%` <- doRNG::`%dorng%`
      `%dopar%` <- foreach::`%dopar%`
      parallel_results = foreach::foreach(
        t = 1L:self$horizon,
        .inorder = TRUE,
        .packages = c("data.table")
      ) %dorng% {
        parallel_counter <- 1L
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {

            context = agent[[a, s]]$get_context()
            action  = agent[[a, s]]$get_action(context)
            reward  = agent[[a, s]]$get_reward(action)
            agent[[a, s]]$set_reward(reward, context)

            self$history$save_step(parallel_counter,
                                   t,
                                   s,
                                   action,
                                   reward,
                                   agent[[a, s]]$policy$name)

            parallel_counter <- parallel_counter + 1L
          }
        }
        dth <- self$history$get_data_table()
        dth[sim != 0]
      }
      parallel_results = data.table::rbindlist(parallel_results)
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
