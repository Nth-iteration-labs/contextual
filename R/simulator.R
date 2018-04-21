#' @import foreach
#' @import doParallel
#' @import itertools
#'
#' @export
Simulator <- R6::R6Class(
  "Simulator",
  portable = FALSE,
  class = FALSE,
  inherit = Contextual,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    number_of_agents = NULL,
    horizon = NULL,
    simulations = NULL,
    worker_max = NULL,
    history = NULL,
    save_context = NULL,
    save_theta = NULL,
    do_parallel = NULL,
    sims_per_agent_list = NULL,
    continouous_counter = NULL,
    set_seed = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = 7,
                          continouous_counter = FALSE,
                          set_seed = 0) {
      self$horizon <- horizon
      self$simulations <- simulations
      self$save_theta <- save_theta
      self$save_context <- save_context
      if (!is.list(agents)) agents <- list(agents)
      self$agents <- agents
      self$number_of_agents <- length(agents)
      self$worker_max <- worker_max
      self$do_parallel <- do_parallel
      self$continouous_counter <- continouous_counter
      self$set_seed <- set_seed
      self$reset()
    },
    reset = function() {
      self$history <- History$new(self$horizon * self$number_of_agents * self$simulations)
      self$sims_per_agent_list <-  matrix(list(), self$simulations, self$number_of_agents)
      # make policy names unique by appending sequence numbers to duplicates.
      policy_name_list <- list()
      for (agent_index in 1L:self$number_of_agents) {

        current_policy_name <- self$agents[[agent_index]]$policy$name
        policy_name_list <- c(policy_name_list,current_policy_name)
        current_policy_name_occurrences <- length(policy_name_list[policy_name_list == current_policy_name])
        if (current_policy_name_occurrences > 1) {
          self$agents[[agent_index]]$policy$name <- paste0(current_policy_name,'.',current_policy_name_occurrences)
        }
      }
      # clone bandits and policies
      for (sim_index in 1L:self$simulations) {
        for (agent_index in 1L:self$number_of_agents) {
          self$sims_per_agent_list[sim_index, agent_index]  <- list(self$agents[[agent_index]]$clone(deep = FALSE))
          self$sims_per_agent_list[[sim_index, agent_index]]$reset()
          self$sims_per_agent_list[[sim_index, agent_index]]$bandit <- self$sims_per_agent_list[[sim_index, agent_index]]$bandit$clone(deep = TRUE)
          self$sims_per_agent_list[[sim_index, agent_index]]$policy <- self$sims_per_agent_list[[sim_index, agent_index]]$policy$clone(deep = FALSE)  ## save theta here if deep, then contextual class gone though
          self$sims_per_agent_list[[sim_index, agent_index]]$sim_index <- sim_index
          self$sims_per_agent_list[[sim_index, agent_index]]$agent_index <- agent_index
        }
      }
    },
    run = function() {
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
      sims_per_agent_list <- self$sims_per_agent_list
      number_of_agents <- self$number_of_agents
      save_context <- self$save_context
      save_theta <- self$save_theta
      continouous_counter <- self$continouous_counter
      set_seed <- self$set_seed
      foreach_results <- foreach::foreach(
        sims_agents = itertools::isplitRows(sims_per_agent_list, chunks = workers),
        i = iterators::icount(),
        .inorder = TRUE,
        .export = c("History"),
        .noexport = c("sims_per_agent_list","history"),
        .packages = c("data.table","itertools","rstan")
      ) %fun% {
        index <- 1L
        local_history <- History$new( horizon * number_of_agents * length(sims_agents), save_context, save_theta )
        for (sim_agent in sims_agents) {
          simulation_index <- sim_agent$sim_index
          policy_name <- sim_agent$policy$name
          set.seed(simulation_index + set_seed*42)
          sim_agent$bandit$pre_calculate()
          if (sim_agent$bandit$precaching ) {
            sim_agent$bandit$generate_bandit_data(n = horizon)
          }
          if (continouous_counter) sim_agent$set_t(as.integer((simulation_index - 1L) * horizon))
          for (t in 1L:horizon) {
            step <- sim_agent$step()
            if (!is.null(step$reward)) {
              local_history$save(
                index,
                t,
                step$action,
                step$reward,
                policy_name,
                simulation_index,
                if (save_context) step$context$X else NA,
                if (save_theta)   step$theta     else NA
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
