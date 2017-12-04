library(R6)
library(progress)
source("../R/utility.R")
library(foreach)
library(doParallel)

#' @export
Simulator <- R6Class(
  "Simulator",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  private = list(
    rewards = NULL
  ),
  public = list(
    agent.list = NULL,
    agent.n = NULL,
    animate = FALSE,
    animate.step = 2,
    horizon = 100L,
    simulations = 1L,
    history = NULL,
    initialize = function(agent.list, animate = FALSE, animate.step = 2) {
      self$agent.list = agent.list
      self$agent.n = length(agent.list)
      self$animate = animate
      self$animate.step = animate.step
      self$reset
    },
    reset = function(){
      for (a in 1L:agent.n) agent.list[[a]]$reset()
    },
    run = function(horizon=100L, simulations=1L) {
      counter = 1L
      self$horizon = horizon
      pb <- progress_bar$new(total = horizon)
      pb$tick(0)
      self$simulations = simulations


      n = self$horizon*self$agent.n*self$simulations
      self$history = History$new(n)
      agent.instance =  matrix( list(), agent.n,simulations)
      bandit.instance = matrix( list(), agent.n,simulations)
      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent.n) {
          agent.instance[a,s]  = list(self$agent.list[[a]]$clone())
          bandit.instance[a,s] = list(self$agent.list[[a]]$bandit$clone())
        }
      }

      registerDoParallel(cores = detectCores())

      foreach(t = 1L:self$horizon) %dopar% {
      #for (t in 1L:self$horizon) {

        pb$tick()
        for (a in 1L:self$agent.n) {

          for (s in 1L:self$simulations) {
            context  = bandit.instance[[a,s]]$get.context()        # context k * d, works at current t for now
            action   = agent.instance[[a,s]]$get.action(context)   # agent chooses an arm k, knowing context d
            reward   = bandit.instance[[a,s]]$get.reward(action)   # see how the bandit rewards us
            agent.instance[[a,s]]$set.reward(reward,context)       # agent adapts theta..
                                                                   # .. knowing reward choice arm k in context

            history$save(counter,t,s,action,reward,agent.instance[[a,s]]$policy$name)
            inc(counter) <- 1L
          }
        }
        if (self$animate == TRUE && t %% animate.step == 0) plot$grid(history$get.data.table()[t != 0L])  # xlim = c(0,horizon))
      }

      return(history)
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

