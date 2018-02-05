#' @import foreach
#' @import doParallel
#' @import itertools
#' @export
Simulator <- R6::R6Class(
  "Simulator",
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
    do_parallel = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = 7) {
      super$initialize()
      self$horizon <- horizon
      self$simulations <- simulations
      self$save_theta <- save_theta
      self$save_context <- save_context
      if (!is.list(agents)) agents <- list(agents)
      self$agents <- agents
      self$worker_max <- worker_max
      self$agent_n <- length(agents)
      self$do_parallel <- do_parallel
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n) {
        self$agents[[a]]$reset()
        if (self$agents[[a]]$bandit$is_precaching ) {
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

      `%fun%` <- foreach::`%do%`
      workers <- 1

      if (self$do_parallel) {
        message("Preworkercreation")

        nr_cores <- parallel::detectCores()
        if (nr_cores >= 3) workers <- nr_cores - 1                              # nocov
        if (workers > worker_max) workers <- worker_max
        cl <- parallel::makeCluster(workers, useXDR = FALSE)                    # type="FORK" only linux
        doParallel::registerDoParallel(cl)
        `%fun%` <- foreach::`%dopar%`

        message("Postworkercreation")
      }

      horizon <- self$horizon
      agent_n <- self$agent_n
      save_context <- self$save_context
      save_theta <- self$save_theta

      self$history <- History$new(  self$horizon *
                                    self$agent_n *
                                    self$simulations )

      foreach_results <- foreach::foreach(
        sims_agents = itertools::isplitRows(sims_agents_list, chunks = workers),
        i = iterators::icount(),
        .inorder = FALSE,
        .export = c("History"),
        .noexport = c("sims_agents_list","history"),
        .packages = c("data.table","itertools")
      ) %fun% {
        index <- 1L
        agent_index <- 21L

        local_history <- History$new( horizon * agent_n * length(sims_agents),
                                      save_context,
                                      save_theta )
        for (sim_agent in sims_agents) {
          simulation_index <- sim_agent$s_index
          set.seed(simulation_index)
          policy_name <- sim_agent$policy$name
          for (t in 1L:horizon) {

            agent_index <- as.integer(t + ((simulation_index - 1L) * horizon))

            context <- sim_agent$bandit_get_context(agent_index)                # observe the bandit in its context
            action  <- sim_agent$policy_get_action(agent_index)                 # use policy to decide which choice to make (which arm to pick)
            reward  <- sim_agent$bandit_get_reward(agent_index)                 # observe the resonse of the bandit in this context
            if (!is.null(reward)) {
              theta <- sim_agent$policy_set_reward(agent_index)                 # adjust the policy, update theta

              local_history$save(
                                   index,                                       # save the results to a history log
                                   t,
                                   action,
                                   reward,
                                   policy_name,
                                   simulation_index,
                                   if (self$save_context) context$X else NA,
                                   if (self$save_theta)   theta     else NA
                               )

              index <- index + 1L
            }
          }
        }
        dth <- local_history$get_data_table()
        dth[sim != 0]
      }
      if (self$do_parallel) {
        parallel::stopCluster(cl)
      }
      foreach_results <- data.table::rbindlist(foreach_results)
      self$history$set_data_table(foreach_results)
      self$history
    },
    object_size = function() {
      cat(paste("Simulator: ", self$hash),"\n")
      cat(paste("  Size of history:    ",
        format(object.size(self$history$data), units = "auto")),"\n")
      cat(paste("    Size of t:        ",
        format(object.size(self$history$data$t), units = "auto")),"\n")
      cat(paste("    Size of sim:      ",
        format(object.size(self$history$data$sim), units = "auto")),"\n")
      cat(paste("    Size of arm:      ",
        format(object.size(self$history$data$arm), units = "auto")),"\n")
      cat(paste("    Size of r:        ",
        format(object.size(self$history$data$reward), units = "auto")),"\n")
      cat(paste("    Size of opt:      ",
        format(object.size(self$history$data$is_optimal), units = "auto")),"\n")
      cat(paste("    Size of oracle:   ",
        format(object.size(self$history$data$oracle), units = "auto")),"\n")
      cat(paste("    Size of agent:    ",
        format(object.size(self$history$data$agent), units = "auto")),"\n")
      cat(paste("    Size of context:  ",
        format(object.size(self$history$data$context), units = "auto")),"\n")
      cat(paste("    Size of theta:    ",
        format(object.size(self$history$data$theta), units = "auto")),"\n")

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
