#' @importFrom foreach %dopar% %do%
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom itertools isplitRows
#' @importFrom data.table rbindlist
#'
#' @export
Simulator <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
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
    continuous_counter = NULL,
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
                          continuous_counter = FALSE,
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
      self$continuous_counter <- continuous_counter
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

      # (re)create history's data.table
      self$history <- History$new(self$horizon * self$number_of_agents * self$simulations)  ###

      self$history$add_meta_data("sim_start_time",format(Sys.time(), "%a %b %d %X %Y"))

      self$sims_per_agent_list <-  matrix(list(), self$simulations, self$number_of_agents)
      # unique policy names through appending sequence numbers to duplicates
      agent_name_list <- list()

      for (agent_index in 1L:self$number_of_agents) {
        current_agent_name <- self$agents[[agent_index]]$name
        agent_name_list <- c(agent_name_list,current_agent_name)
        current_agent_name_occurrences <- length(agent_name_list[agent_name_list == current_agent_name])
        if (current_agent_name_occurrences > 1) {
          self$agents[[agent_index]]$name <- paste0(current_agent_name,'.',current_agent_name_occurrences)
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
      # copy relevant variables to local environment
      horizon <- self$horizon
      sims_per_agent_list <- self$sims_per_agent_list
      number_of_agents <- self$number_of_agents
      save_context <- self$save_context
      save_theta <- self$save_theta
      reindex_t <- self$reindex_t
      write_progress_file <- self$write_progress_file
      continuous_counter <- self$continuous_counter
      set_seed <- self$set_seed
      # calculate chunk size
      sa_iterator <- itertools::isplitRows(sims_per_agent_list, chunks = workers)
      # include packages that are used in parallel processes
      par_packages <- c(c("data.table","itertools"),include_packages)
      # running the main simulation loop
      private$start_time = Sys.time()
      foreach_results <- foreach::foreach(
        sims_agents = sa_iterator,
        i = iterators::icount(),
        .inorder = TRUE,
        .combine = function(x,y)data.table::rbindlist(list(x,y)),
        .export = c("History"),
        .noexport = c("sims_per_agent_list","history"),
        .packages = par_packages
      ) %fun% {
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
          agent_name <- sim_agent$name
          local_curent_seed <- simulation_index + set_seed*42
          set.seed(local_curent_seed)
          sim_agent$bandit$pre_calculate()
          if (sim_agent$bandit$precaching ) {
            sim_agent$bandit$generate_bandit_data(n = horizon)
          }
          if (continuous_counter) sim_agent$set_t(as.integer((simulation_index - 1L) * horizon))
          for (t in 1L:horizon) {
            step <- sim_agent$do_step()
            if (!is.null(step$reward)) {
              local_history$save(
                index,
                t,
                step$action,
                step$reward,
                agent_name,
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
      self$history$set_data_table(foreach_results)
      private$end_time = Sys.time()
      if (reindex_t) self$history$reindex_t()
      agent_meta_to_history()
      self$history$calculate_stats()
      self$history$add_meta_data("sim_end_time",format(Sys.time(), "%a %b %d %X %Y"))
      self$history$add_meta_data("sim_total_duration",private$end_time - private$start_time)
      self$history
    },
    finalize = function() {
      if (self$do_parallel) {
        try({
          parallel::stopCluster(self$cl)
        })
        doParallel::stopImplicitCluster()
        # Making sure that we are closing all processes that were
        # spawned but (potentially) not terminated by the foreach loop.
        closeAllConnections()
      }
    }
  ),
  private = list(
    start_time = NULL,
    end_time = NULL,
    agent_meta_to_history = function() {
      self$history$initialize_meta_agent()
      #agent_policy_call_list <- list()
      #agent_bandit_call_list <- list()
      #for (agent_index in 1L:self$number_of_agents) {
      #  current_agent_name <- self$agents[[agent_index]]$name
      #  agent_policy_call_list[[current_agent_name]] <- self$agents[[agent_index]]$policy_call
      #  agent_bandit_call_list[[current_agent_name]] <- self$agents[[agent_index]]$bandit_call
      #}
      #self$history$add_agent_data("policy_call",agent_policy_call_list)
      #self$history$add_agent_data("bandit_call",agent_bandit_call_list)
    }
  )
)


#' Simulator
#'
#' The R6 class \code{Simulator} is the entry point of any \pkg{contextual}
#' simulation. It encapsulates one or more \code{Agents}, clones them if necessary,
#' runs the \code{Agents} (in parallel, by default), and saves the log of all of the \code{Agents}
#' interactions to a \code{History} object.
#'
#' @name Simulator
#' @aliases run
#'
#' @section Usage:
#' \preformatted{
#' simulator <- Simulator$new(agents,
#'                            horizon = 100L,
#'                            simulations = 100L,
#'                            save_context = FALSE,
#'                            save_theta = FALSE,
#'                            do_parallel = TRUE,
#'                            worker_max = NULL,
#'                            continuous_counter = FALSE,
#'                            set_seed = 0,
#'                            write_progress_file = TRUE,
#'                            include_packages = NULL,
#'                            reindex_t = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{agents}}{
#'     An \code{Agent} instance, or a \code{list} of \code{Agent} instances to be run
#'     by the instantiated \code{Simulator}.
#'   }
#'   \item{\code{horizon}}{
#'     \code{integer}. The T time steps to run the instantiated \code{Simulator}, where \emph{t} = \{1, \ldots, T\}.
#'   }
#'   \item{\code{simulations}}{
#'     \code{integer}. How many times to repeat each agent's simulation over \emph{t} = \{1, \ldots, T\},
#'     with a new seed on each repeat (itself deterministically derived from set\_seed).
#'   }
#'   \item{\code{save_context}}{
#'     \code{logical}. Save the context matrices \code{X} to the History log during a simulation?
#'   }
#'   \item{\code{save_theta}}{
#'     \code{logical}. Save the parameter list \code{theta} to the History log during a simulation?
#'   }
#'   \item{\code{do_parallel}}{
#'      \code{logical}. Run \code{Simulator} processes in parallel?
#'   }
#'   \item{\code{worker_max}}{
#'      \code{integer}. Specifies how many parallel workers are to be used, when \code{do_parallel}
#'      is \code{TRUE}. If unspecified, the amount of workers defaults to \code{max(workers_available)-1}.
#'
#'   }
#'   \item{\code{continuous_counter}}{
#'      \code{logical}. Of use to, amongst others, offline Bandits.
#'      If \code{continuous_counter} is set to \code{TRUE}, the current \code{Simulator}
#'      iterates over all rows in a data set for each repeated simulation.
#'      If \code{FALSE}, it splits the data into \code{simulations} parts,
#'      and a different subset of the data for each repeat of an agent's simulation.
#'   }
#'   \item{\code{set_seed}}{
#'      \code{integer}. Sets the seed of R‘s random number generator for the current \code{Simulator}.
#'   }
#'   \item{\code{write_progress_file}}{
#'       \code{logical}. If \code{TRUE}, \code{Simulator} writes \code{progress.log} and \code{doparallel.log}
#'       files to the current working directory, allowing you to keep track of \code{workers}, iterations,
#'       and potential errors when running a \code{Simulator} in parallel.
#'   }
#'   \item{\code{include_packages}}{
#'       \code{List}. List of packages that (one of) the policies depend on. If a \code{Policy} requires an
#'       R package to be loaded, this option can be used to load that package on each of the workers.
#'       Ignored if \code{do_parallel} is \code{FALSE}.
#'   }
#'   \item{\code{reindex_t}}{
#'      \code{logical}. If \code{TRUE}, removes empty rows from the \code{History} log,
#'      reindexes the \code{t} column, and truncates the resulting data to the shortest simulation
#'      grouped by agent and simulation.
#'   }
#'
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{reset()}}{
#'      Resets a \code{Simulator} instance to its original initialisation values.
#'   }
#'
#'   \item{\code{run()}}{
#'      Runs a \code{Simulator} instance.
#'    }
#'
#'  }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{ContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
