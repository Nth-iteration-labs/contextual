#' @import foreach
#' @import doParallel
#' @import itertools
#'
#' @export
Simulator <- R6::R6Class(
  "Simulator",
  portable = FALSE,
  class = FALSE,
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
    write_progress_file = NULL,
    include_packages = NULL,
    cl = NULL,
    reindex_t = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = NULL,
                          continouous_counter = FALSE,
                          set_seed = 0,
                          write_progress_file = TRUE,
                          include_packages = NULL,
                          reindex_t = FALSE) {
      self$reindex_t <- reindex_t
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
      self$write_progress_file <- write_progress_file
      self$include_packages <- include_packages
      self$reset()
    },
    reset = function() {
      set.seed(self$set_seed)

      # create empty progress.log file
      if (self$write_progress_file) cat(paste0(""), file = "progress.log", append = FALSE)
      # clear doparallel.log
      if (self$write_progress_file) cat(paste0(""), file = "doparallel.log", append = FALSE)

      self$history <- History$new(self$horizon * self$number_of_agents * self$simulations)
      self$sims_per_agent_list <-  matrix(list(), self$simulations, self$number_of_agents)
      # make policy names unique by appending sequence numbers to duplicates
      policy_name_list <- list()
      for (agent_index in 1L:self$number_of_agents) {

        current_policy_name <- self$agents[[agent_index]]$policy$name
        policy_name_list <- c(policy_name_list,current_policy_name)
        current_policy_name_occurrences <- length(policy_name_list[policy_name_list == current_policy_name])
        if (current_policy_name_occurrences > 1) {
          self$agents[[agent_index]]$policy$name <- paste0(current_policy_name,'.',current_policy_name_occurrences)
        }
      }
      # clone, precache and precalculate bandits and policies where relevant
      message("Cloning, precaching and precalculating bandits and policies")
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
      # run foreach either parallel or not, create workers
      `%fun%` <- foreach::`%do%`
      workers <- 1
      if (self$do_parallel) {
        message("Preworkercreation")
        nr_cores <- parallel::detectCores()
        if (nr_cores >= 3) workers <- nr_cores - 1                              # nocov
        if (!is.null(worker_max)) {
          if (workers > worker_max) workers <- worker_max
        }
        if (grepl('w|W', .Platform$OS.type)) {
          # Windows
          self$cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "PSOCK", methods = FALSE, port = 11999, outfile = "doparallel.log")
        } else {
          # Linux or MacOS
          # Problem with fork, seems to pup up irregularly:
          # cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "FORK", methods=FALSE, port=11999, outfile = "doparallel.log")
          # https://stackoverflow.com/questions/9486952/remove-zombie-processes-using-parallel-package
          # so to make sure we are ok, we use PSOCK everywhere for now:
          self$cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "PSOCK", methods = FALSE, port = 11999, outfile = "doparallel.log")
          if (grepl('darwin', version$os)) {
            # macOS - potential future osx/linux specific implementation settings go here
          } else {
            # Linux - potential future osx/linux specific implementation settings go here
          }
        }
        message(paste0("Cores available: ",nr_cores))
        message(paste0("Workers assigned: ",workers))
        doParallel::registerDoParallel(self$cl)
        `%fun%` <- foreach::`%dopar%`
        message("Postworkercreation")
      }
      # copy some variables to local scope
      horizon <- self$horizon
      sims_per_agent_list <- self$sims_per_agent_list
      number_of_agents <- self$number_of_agents
      save_context <- self$save_context
      save_theta <- self$save_theta
      reindex_t <- self$reindex_t
      write_progress_file <- self$write_progress_file
      continouous_counter <- self$continouous_counter
      set_seed <- self$set_seed
      sa_iterator <- itertools::isplitRows(sims_per_agent_list, chunks = workers)
      par_packages <- c(c("data.table","itertools"),include_packages)
      # running the main simulation loop
      foreach_results <- foreach::foreach(
        sims_agents = sa_iterator,
        i = iterators::icount(),
        .inorder = TRUE,
        .combine = function(x,y)rbindlist(list(x,y)),
        .export = c("History"),
        .noexport = c("sims_per_agent_list","history"),
        .packages = par_packages
      ) %fun% {
        # parallelize the longest running part, here sim from t=1 till the horizon
        index <- 1L
        sim_agent_counter <- 0
        sim_agent_total <- length(sims_agents)
        local_history <- History$new( horizon * number_of_agents * sim_agent_total, save_context, save_theta)
        for (sim_agent in sims_agents) {
          sim_agent_counter <- sim_agent_counter + 1
          if (write_progress_file) {
            cat(paste0(format(Sys.time(), format = "%H:%M:%OS6"),
                       ", Worker: ", i,
                       ", Sim: ", sim_agent_counter,
                       " of ", sim_agent_total,"\n"),
                file = "progress.log", append = TRUE)
          }
          simulation_index <- sim_agent$sim_index
          policy_name <- sim_agent$policy$name
          local_curent_seed <- simulation_index + set_seed*42
          set.seed(local_curent_seed)
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
        #parallel::stopCluster(cl)
      }
      self$history$set_data_table(foreach_results)
      if(reindex_t) self$history$reindex_t()
      self$history
    },
    finalize = function() {
      if (self$do_parallel) {
        try({
          parallel::stopCluster(self$cl)
        })
        stopImplicitCluster()
        # Making sure that we are closing all processes that were
        # spawned but not terminated by the foreach loop.
        closeAllConnections()
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
