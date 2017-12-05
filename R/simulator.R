library(R6)
library(foreach)
library(doParallel)
library(doRNG)

#' @import foreach
#' @import doParallel
#' @import doRNG

#' @export
Simulator <- R6Class(
  "Simulator",
  inherit = Contextual,
  portable = FALSE, class = FALSE, cloneable = FALSE,
  private = list(rewards = NULL),
  public = list(
    agent.list = NULL,
    agent.n = NULL,
    animate = FALSE,
    animate.step = 2,
    horizon = 100L,
    simulations = 1L,
    history = NULL,
    parallel = FALSE,
    initialize = function(agent.list, animate = FALSE, animate.step = 2, parallel = FALSE) {
      self$history = History$new()
      self$agent.list = agent.list
      self$agent.n = length(agent.list)
      self$animate = animate
      self$animate.step = animate.step
      self$parallel = parallel
      self$reset()
    },
    reset = function() {
      for (a in 1L:agent.n)
        agent.list[[a]]$reset()
    },
    run = function(horizon = 100L, simulations = 100L) {

      self$horizon = horizon
      self$simulations = simulations
      agent.instance =  matrix(list(), agent.n, simulations)
      bandit.instance = matrix(list(), agent.n, simulations)
      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent.n) {
          agent.instance[a, s]  = list(self$agent.list[[a]]$clone())            #deep? in init?
          bandit.instance[a, s] = list(self$agent.list[[a]]$bandit$clone())
        }
      }

      if (!self$parallel) {

        counter = 1L
        n = self$horizon * self$agent.n * self$simulations
        self$history$reset(n)
        for (t in 1L:self$horizon) {
          for (a in 1L:self$agent.n) {
            for (s in 1L:self$simulations) {

              context  = bandit.instance[[a, s]]$get.context()        # context k * d, works at current t for now
              action   = agent.instance[[a, s]]$get.action(context)   # agent chooses an arm k, knowing context d
              reward   = bandit.instance[[a, s]]$get.reward(action)   # see how the bandit rewards us
              agent.instance[[a, s]]$set.reward(reward, context)      # agent adapts theta knowing reward choice arm k in context

              self$history$save.step(counter,t,s,action,reward,agent.instance[[a, s]]$policy$name)
              counter <- counter + 1L
            }
          }
          if (self$animate == TRUE && t %% animate.step == 0) plot$grid(history$get.data.table()[t != 0L])  # xlim = c(0,horizon))
        }
        return(history$get.data.table())

      } else {

        workers <- detectCores() - 1
        cl <- makeCluster(workers)
        registerDoParallel(cl)
        n = as.integer(ceiling(self$horizon * self$agent.n * self$simulations / workers))
        self$history$reset(n)
        parallel_results = foreach(t = 1L:self$horizon, .inorder = TRUE, .packages = c("data.table")) %dorng% {
          parallel_counter <- 1L
          for (a in 1L:self$agent.n) {
            for (s in 1L:self$simulations) {
              context  = bandit.instance[[a, s]]$get.context()        # context k * d, works at current t for now
              action   = agent.instance[[a, s]]$get.action(context)   # agent chooses an arm k, knowing context d
              reward   = bandit.instance[[a, s]]$get.reward(action)   # see how the bandit rewards us
              agent.instance[[a, s]]$set.reward(reward, context)      # agent adapts theta knowing reward choice arm k in context
              self$history$save.step(parallel_counter,t,s,action,reward,agent.instance[[a, s]]$policy$name)
              parallel_counter <- parallel_counter + 1L
            }
          }
          self$history$get.data.table()
        }
        parallel_results = rbindlist(parallel_results)[sim != 0]
        #self$history$set.data.table(parallel_results)
        stopCluster(cl)
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
