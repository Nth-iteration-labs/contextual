#' @importFrom foreach %dopar% %do%
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom itertools isplitRows
#' @importFrom data.table rbindlist
#' @importFrom iterators icount
#'
#' @export
Simulator <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  public = list(
    agents = NULL,
    agent_count = NULL,
    horizon = NULL,
    simulations = NULL,
    worker_max = NULL,
    history = NULL,
    save_context = NULL,
    save_theta = NULL,
    do_parallel = NULL,
    sims_and_agents_list = NULL,
    t_over_sims = NULL,
    set_seed = NULL,
    progress_file = NULL,
    log_interval = NULL,
    include_packages = NULL,
    cl = NULL,
    reindex = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = NULL,
                          set_seed = 0,
                          progress_file = FALSE,
                          log_interval = 1000,
                          include_packages = NULL,
                          t_over_sims = FALSE,
                          reindex = FALSE) {

      if (!is.list(agents)) agents <- list(agents)

      self$reindex <- reindex
      self$progress_file <- progress_file
      self$log_interval <- log_interval
      self$horizon <- horizon
      self$simulations <- simulations
      self$save_theta <- save_theta
      self$save_context <- save_context
      self$agents <- agents
      self$agent_count <- length(agents)
      self$worker_max <- worker_max
      self$do_parallel <- do_parallel
      self$t_over_sims <- t_over_sims
      self$set_seed <- set_seed
      self$include_packages <- include_packages

      self$reset()
    },
    reset = function() {
      set.seed(self$set_seed)

      # create or clear progress.log file
      if (self$progress_file) cat(paste0(""), file = "progress.log", append = FALSE)

      # create or clear parallel.log file
      if (self$progress_file) cat(paste0(""), file = "parallel.log", append = FALSE)

      # (re)create history's data.table
      self$history <- History$new(self$horizon * self$agent_count * self$simulations)

      self$history$set_meta_data("horizon",self$horizon)
      self$history$set_meta_data("agents",self$agent_count)
      self$history$set_meta_data("simulations",self$simulations)

      self$history$set_meta_data("sim_start_time",format(Sys.time(), "%a %b %d %X %Y"))
      self$sims_and_agents_list <- vector("list", self$simulations*self$agent_count)
      # unique policy names through appending sequence numbers to duplicates
      agent_name_list <- list()

      for (agent_index in 1L:self$agent_count) {
        current_agent_name <- self$agents[[agent_index]]$name
        agent_name_list <- c(agent_name_list,current_agent_name)
        current_agent_name_occurrences <- length(agent_name_list[agent_name_list == current_agent_name])
        if (current_agent_name_occurrences > 1) {
          self$agents[[agent_index]]$name <- paste0(current_agent_name,'.',current_agent_name_occurrences)
        }
      }

      # clone, precache and precalculate bandits and policies where relevant
      message("Cloning, precaching and precalculating bandits and policies")
      index <- 1
      for (sim_index in 1L:self$simulations) {
        for (agent_index in 1L:self$agent_count) {
          self$sims_and_agents_list[index]  <- list(self$agents[[agent_index]]$clone(deep = FALSE))
          self$sims_and_agents_list[[index]]$reset()
          self$sims_and_agents_list[[index]]$bandit <-
            self$sims_and_agents_list[[index]]$bandit$clone(deep = TRUE)
          self$sims_and_agents_list[[index]]$policy <-
            self$sims_and_agents_list[[index]]$policy$clone(deep = FALSE)
          self$sims_and_agents_list[[index]]$sim_index <- sim_index
          self$sims_and_agents_list[[index]]$agent_index <- agent_index
          index <- index + 1
        }
      }

    },
    run = function() {
      # run foreach either parallel or not, create workers
      `%fun%` <- foreach::`%do%`
      workers <- 1
      # parallel tests do not do well on CRAN, so no test coverage for parallel section
      # nocov start
      if (self$do_parallel) {
        message("Preworkercreation")
        nr_cores <- parallel::detectCores()
        if (nr_cores >= 3) workers <- nr_cores - 1
        if (!is.null(worker_max)) {
          if (workers > worker_max) workers <- worker_max
        }
        # clean up any leftover processes
        doParallel::stopImplicitCluster()
        if (grepl('w|W', .Platform$OS.type)) {

          # Windows

          self$cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "PSOCK",
                                           methods = FALSE, setup_timeout = 30, outfile = "parallel.log")
        } else {
          # Linux or MacOS
          # self$cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "FORK", methods=FALSE,
          #                                   port=11999, outfile = "parallel.log")

          # There are issues with FORK that pop up irregularly and have proven hard to pin down.
          # So to make sure sims work, we use PSOCK for all operating systems - for now.
          self$cl <- parallel::makeCluster(workers, useXDR = FALSE, type = "PSOCK",
                                           methods = FALSE, setup_timeout = 30, outfile = "parallel.log")
          if (grepl('darwin', version$os)) {
            # macOS - potential future osx/linux specific settings go here.
          } else {
            # Linux - potential future osx/linux specific settings go here.
          }
        }
        message(paste0("Cores available: ",nr_cores))
        message(paste0("Workers assigned: ",workers))
        doParallel::registerDoParallel(self$cl)
        `%fun%` <- foreach::`%dopar%`
        message("Postworkercreation")
      }
      # If Microsoft R, set MKL threads to 1
      try({library(RevoUtilsMath);RevoUtilsMath::setMKLthreads(1);}, silent = TRUE)
      # nocov end

      # copy relevant variables to local environment
      horizon <- self$horizon
      sims_and_agents_list <- self$sims_and_agents_list
      agent_count <- self$agent_count
      save_context <- self$save_context
      save_theta <- self$save_theta
      reindex <- self$reindex
      progress_file <- self$progress_file
      log_interval <- self$log_interval
      t_over_sims <- self$t_over_sims
      set_seed <- self$set_seed
      # calculate chunk size
      if (length(sims_and_agents_list) <= workers) {
        nr_of_chunks <- length(sims_and_agents_list)
      } else {
        nr_of_chunks <- workers
      }
      # split sim vector into chuncks
      sa_iterator <- itertools::isplitVector(sims_and_agents_list, chunks = nr_of_chunks)
      # include packages that are used in parallel processes
      par_packages <- c(c("data.table","iterators","itertools"),include_packages)
      # running the main simulation loop
      private$start_time <- Sys.time()
      foreach_results <- foreach::foreach(
        sims_agents = sa_iterator,
        i = iterators::icount(),
        .inorder = TRUE,
        .export = c("History"),
        .noexport = c("sims_and_agents_list","history","sa_iterator"),
        .packages = par_packages
      ) %fun% {
        index <- 1L
        sim_agent_counter <- 0
        sim_agent_total <- length(sims_agents)
        local_history <- History$new( horizon * sim_agent_total, save_context, save_theta)
        for (sim_agent in sims_agents) {
          sim_agent_counter <- sim_agent_counter + 1
          if (isTRUE(progress_file)) {
            sim_agent$progress_file <- TRUE
            sim_agent$log_interval <- log_interval
            cat(paste0("[",format(Sys.time(), format = "%H:%M:%OS6"),"] ",
                       "        0 > init - ",sprintf("%-20s", sim_agent$name),
                       " worker ", i,
                       " at sim ", sim_agent_counter,
                       " of ", sim_agent_total,"\n"),
                file = "progress.log", append = TRUE)
          }
          simulation_index <- sim_agent$sim_index
          agent_name <- sim_agent$name
          local_curent_seed <- simulation_index + set_seed*42
          set.seed(local_curent_seed)
          sim_agent$bandit$post_initialization()
          if (sim_agent$bandit$precaching ) {
            sim_agent$bandit$generate_bandit_data(n = horizon)
          }
          if (t_over_sims) sim_agent$set_t(as.integer((simulation_index - 1L) * horizon))
          step <- list()

          for (t in 1L:horizon) {
            step <- sim_agent$do_step()
            if (!is.null(step[[3]])) {                         #reward
              local_history$insert(
                index,
                t,
                step[[2]],                                     #action
                step[[3]],                                     #reward
                agent_name,
                simulation_index,
                if (save_context) step[[1]][["X"]] else NA,    #context
                if (save_theta) step[[4]] else NA              #theta
              )
              index <- index + 1L
            }
          }
        }
        sim_agent$bandit$close
        local_history$get_data_table()
      }
      foreach_results <- rbindlist(foreach_results)
      self$history$set_data_table(foreach_results, auto_stats = FALSE)
      private$end_time <- Sys.time()
      if (reindex) self$history$reindex()
      self$history$update_statistics()
      self$history$set_meta_data("sim_end_time",format(Sys.time(), "%a %b %d %X %Y"))
      self$history$set_meta_data("sim_total_duration",
                                 formatted_difftime(private$end_time - private$start_time))
      self$close()
      self$history
    },
    close = function() {
      # nocov start
      if (self$do_parallel) {
        try({
          parallel::stopCluster(self$cl)
        })
        doParallel::stopImplicitCluster()
      }
      # nocov end
    },
    finalize = function() {
      #closeAllConnections()
    }
  ),
  private = list(
    start_time = NULL,
    end_time = NULL
  )
)


#' Simulator
#'
#' The entry point of any \pkg{contextual} simulation.
#'
#' A Simulator takes, at a minimum, one or more \code{\link{Agent}} instances, a horizon
#' (the length of an invidiual simulation, \emph{t} = \{1, \ldots, T\}) and the number of simulations
#' (How many times to repeat each simulation over \emph{t} = \{1, \ldots, T\}, with a new seed
#' on each repeat*).
#'
#' It then runs all simulations (in parallel by default), keeping a log of all \code{\link{Policy}} and
#' \code{\link{Bandit}} interactions in a \code{\link{History}} instance.
#'
#' &ast; Note: to be able to fairly evaluate and compare each agent's performance, and to make sure that
#' simulations are replicable, for each seperate agent, seeds are set equally and deterministically for
#' each agent over all \code{horizon x simulations} time steps.
#'
#' ![](1_simulator.jpeg "contextual diagram: simulator")
#'
#' @name Simulator
#' @aliases run simulator
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
#'                            t_over_sims = FALSE,
#'                            set_seed = 0,
#'                            progress_file = FALSE,
#'                            include_packages = NULL,
#'                            reindex = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{agents}}{
#'     An \code{Agent} instance or a \code{list} of \code{Agent} instances.
#'   }
#'   \item{\code{horizon}}{
#'     \code{integer}. The number of pulls or time steps to run each agent, where \emph{t} = \{1, \ldots, T\}.
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
#'      \code{integer}. Specifies how many parallel workers are to be used.
#'      If unspecified, the amount of workers defaults to \code{max(workers_available)-1}.
#'
#'   }
#'   \item{\code{t_over_sims}}{
#'      \code{logical}. Of use to, amongst others, offline Bandits.
#'      If \code{t_over_sims} is set to \code{TRUE}, the current \code{Simulator}
#'      iterates over all rows in a data set for each repeated simulation.
#'      If \code{FALSE}, it splits the data into \code{simulations} parts,
#'      and a different subset of the data for each repeat of an agent's simulation.
#'   }
#'   \item{\code{set_seed}}{
#'      \code{integer}. Sets the seed of R‘s random number generator for the current \code{Simulator}.
#'   }
#'   \item{\code{progress_file}}{
#'       \code{logical}. If \code{TRUE}, \code{Simulator} writes \code{progress.log} and \code{parallel.log}
#'       files to the current working directory, allowing you to keep track of \code{workers}, iterations,
#'       and potential errors when running a \code{Simulator} in parallel.
#'   }
#'   \item{\code{include_packages}}{
#'       \code{List}. List of packages that (one of) the policies depend on. If a \code{Policy} requires an
#'       R package to be loaded, this option can be used to load that package on each of the workers.
#'       Ignored if \code{do_parallel} is \code{FALSE}.
#'   }
#'   \item{\code{reindex}}{
#'      \code{logical}. If \code{TRUE}, removes empty rows from the \code{History} log,
#'      reindexes the \code{t} column, and truncates the resulting data to the shortest simulation
#'      grouped by agent and simulation.
#'   }
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
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
NULL
