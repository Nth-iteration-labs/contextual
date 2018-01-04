# TODO: make sure same results (SEED) in par. and serial

#' @import foreach
#' @import doParallel
#' @import itertools
#' @export
SimulatorParallel <- R6::R6Class(
  "SimulatorParallel",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agent_n = NULL,
    horizon = NULL,
    simulations = NULL,
    worker_max = NULL,
    history = NULL,
    save_context = NULL,
    save_theta = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          worker_max = 7) {
      super$initialize()
      self$horizon <- horizon
      self$simulations <- simulations
      self$save_theta <- save_theta
      self$save_context <- save_context
      if (!is.list(agents)) agents = list(agents)
      self$agents <- agents
      self$worker_max <- worker_max
      self$agent_n <- length(agents)
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n) {
        self$agents[[a]]$reset()
        if (self$agents[[a]]$bandit$is_precaching){
          self$agents[[a]]$generate_cache(self$horizon*self$simulations)
        }
      }
    },
    run = function() {
      sims_agents_list <-  matrix(list(), self$simulations, self$agent_n)
      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          sims_agents_list[s, a]  <- list(self$agents[[a]]$clone(deep = FALSE))
          sims_agents_list[[s, a]]$s_index <- s
          sims_agents_list[[s, a]]$a_index <- a
        }
      }
      print("preworkercreation")
      workers <- parallel::detectCores() - 1
      if (workers > worker_max) workers <- worker_max
      cl <- parallel::makeCluster(workers, useXDR = FALSE) #type="FORK")   #  type="FORK" only linux
      print("postworkercreation")
      doParallel::registerDoParallel(cl)

      `%do%` <- foreach::`%do%`
      `%dopar%` <- foreach::`%dopar%`

      horizon = self$horizon
      agent_n = self$agent_n
      save_context = self$save_context
      save_theta = self$save_theta

      self$history <- History$new(self$horizon * self$agent_n * self$simulations)

      parallel_results <- foreach::foreach(
        sims_agents = itertools::isplitRows(sims_agents_list, chunks = workers),
        i = iterators::icount(),
        .inorder = FALSE,
        .export = c("History"),
        .noexport = c("sims_agents_list","history"),
        .packages = c("data.table","itertools")
      ) %dopar% {
        counter <- 1L
        agent_counter <- 21L

        local_history <- History$new(horizon * agent_n * length(sims_agents))

        for (sa in sims_agents) {
          sidx <- sa$s_index
          set.seed(sidx)
          pname <- sa$policy$name
          for (t in 1L:horizon) {

            agent_counter = as.integer(t + ((sidx - 1L) * horizon))

            context <- sa$bandit_get_context(agent_counter)                     # observe the bandit in its context
            action  <- sa$policy_get_decision(agent_counter)                    # use policy to decide which choice to make (which arm to pick)
            reward  <- sa$bandit_get_reward(agent_counter)                      # observe the resonse of the bandit in this context
            if (!is.null(reward)) {
              theta <- sa$policy_set_reward(agent_counter)                      # adjust the policy, update theta
              local_history$save_agent(
                                       counter,                                 # save the results to the history log
                                       t,
                                       action,
                                       reward,
                                       pname,
                                       sidx,
                                       if (save_context) context$X else NA,
                                       if (save_theta)   theta     else NA
                                      )
              counter <- counter + 1L
            }
          }
        }
        dth <- local_history$get_data_table()
        dth[sim != 0]
      }
      parallel_results <- data.table::rbindlist(parallel_results)
      self$history$set_data_table(parallel_results)  ## set it here
      parallel::stopCluster(cl)
      parallel_results   ## return it here .. hmm!
    },
    object_size = function() {
      cat(paste("Simulator: ", self$hash),"\n")
      cat(paste("  Size of history:    ", format(object.size(self$history$data), units = "auto")),"\n")
      cat(paste("    Size of t:        ", format(object.size(self$history$data$t), units = "auto")),"\n")
      cat(paste("    Size of sim:      ", format(object.size(self$history$data$sim), units = "auto")),"\n")
      cat(paste("    Size of arm:      ", format(object.size(self$history$data$arm), units = "auto")),"\n")
      cat(paste("    Size of r:        ", format(object.size(self$history$data$reward), units = "auto")),"\n")
      cat(paste("    Size of opt:      ", format(object.size(self$history$data$optimal), units = "auto")),"\n")
      cat(paste("    Size of agent:    ", format(object.size(self$history$data$agent), units = "auto")),"\n")
      cat(paste("    Size of context:  ", format(object.size(self$history$data$context), units = "auto")),"\n")
      cat(paste("    Size of theta:    ", format(object.size(self$history$data$theta), units = "auto")),"\n")

      agent_hashes <- list()
      for (a in 1L:self$agent_n) {
        if (!(self$agents[[a]]$bandit$hash %in% agent_hashes)) {
          agent_hashes <- list(agent_hashes,self$agents[[a]]$bandit$hash)
          self$agents[[a]]$object_size()
        }
      }
      invisible(self)
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
