#' @import foreach
#' @import doParallel
#' @import doRNG
#' @import data.table

#' @export
Simulator <- R6::R6Class(
  "Simulator",

  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  private = list(rewards = NULL),

  public = list(
    agent_list = NULL,
    agent_n = NULL,
    animate = FALSE,
    animate_step = 2,
    horizon = 100L,
    simulations = 1L,
    history = NULL,
    parallel = FALSE,

    initialize = function(agent_list,
                          animate = FALSE,
                          animate_step = 2,
                          parallel = FALSE) {
      self$history = History$new()
      self$agent_list = agent_list
      self$agent_n = length(agent_list)
      self$animate = animate
      self$animate_step = animate_step
      self$parallel = parallel
      self$reset()
    },
    reset = function() {
      for (a in 1L:agent_n)
        agent_list[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {
      self$horizon = horizon
      self$simulations = simulations
      agent_instance =  matrix(list(), agent_n, simulations)
      bandit_instance = matrix(list(), agent_n, simulations)
      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent_instance[a, s]  = list(self$agent_list[[a]]$clone())            #deep? in init?
          bandit_instance[a, s] = list(self$agent_list[[a]]$bandit$clone())
        }
      }

      if (!self$parallel) {
        counter = 1L
        n = self$horizon * self$agent_n * self$simulations
        self$history$reset(n)
        for (t in 1L:self$horizon) {
          for (a in 1L:self$agent_n) {
            for (s in 1L:self$simulations) {

              context  = bandit_instance[[a, s]]$get_context()
              action   = agent_instance[[a, s]]$get_action(context)
              reward   = bandit_instance[[a, s]]$get_reward(action)
              agent_instance[[a, s]]$set_reward(reward, context)

              self$history$save_step(counter,
                                     t,
                                     s,
                                     action,
                                     reward,
                                     agent_instance[[a, s]]$policy$name)

              counter <- counter + 1L
            }
          }
          if (self$animate == TRUE && t %% animate_step == 0) {
            plot$plot_grid(history$get_data_table()[t != 0L])
          }
        }
        return(history$get_data_table())

      } else {
        workers <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(workers)
        doParallel::registerDoParallel(cl)
        n = as.integer(ceiling(self$horizon / workers *
                                 self$agent_n *
                                 self$simulations))
        self$history$reset(n)
        parallel_results = foreach::foreach(
          t = 1L:self$horizon,
          .inorder = TRUE,
          .packages = c("data.table")
        ) %dorng% {
          parallel_counter <- 1L
          for (a in 1L:self$agent_n) {
            for (s in 1L:self$simulations) {

              context  = bandit_instance[[a, s]]$get_context()
              action   = agent_instance[[a, s]]$get_action(context)
              reward   = bandit_instance[[a, s]]$get_reward(action)
              agent_instance[[a, s]]$set_reward(reward, context)

              self$history$save_step(parallel_counter,
                                     t,
                                     s,
                                     action,
                                     reward,
                                     agent_instance[[a, s]]$policy$name)

              parallel_counter <- parallel_counter + 1L
            }
          }
          self$history$get_data_table()
        }
        parallel_results = rbindlist(parallel_results)[sim != 0]
        self$history$set_data_table(parallel_results)
        parallel::stopCluster(cl)
        return(parallel_results)
      }
    }
  )
)

#' External Simulator
#'
#' Simulator intro
#'
#' @section Usage:
#' \preformatted{b <- Simulator$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Simulator} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Simulator, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Simulator
#' @examples
#'\dontrun{}
#'
NULL
