#' @importFrom foreach %dopar% %do% foreach
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom itertools isplitVector
#' @importFrom data.table rbindlist
#' @importFrom iterators icount
#' @import Formula
#' @export
Simulator <- R6::R6Class(
  "Simulator",
  class = FALSE,
  public = list(
    agents = NULL,
    workers = NULL,
    agent_count = NULL,
    horizon = NULL,
    simulations = NULL,
    worker_max = NULL,
    internal_history = NULL,
    save_context = NULL,
    save_theta = NULL,
    do_parallel = NULL,
    sims_and_agents_list = NULL,
    t_over_sims = NULL,
    set_seed = NULL,
    progress_file = NULL,
    log_interval = NULL,
    save_interval = NULL,
    include_packages = NULL,
    outfile = NULL,
    chunk_multiplier = NULL,
    cl = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = NULL,
                          set_seed = 0,
                          save_interval = 1,
                          progress_file = FALSE,
                          log_interval = 1000,
                          include_packages = NULL,
                          t_over_sims = FALSE,
                          chunk_multiplier = 1) {

      if (!is.list(agents)) agents <- list(agents)

      self$progress_file <- progress_file
      self$log_interval <- as.integer(log_interval)
      self$horizon <- as.integer(horizon)
      self$simulations <- as.integer(simulations)
      self$save_theta <- save_theta
      self$save_context <- save_context
      self$agents <- agents
      self$agent_count <- length(agents)
      self$worker_max <- worker_max
      self$do_parallel <- do_parallel
      self$t_over_sims <- t_over_sims
      self$set_seed <- set_seed
      self$save_interval <- as.integer(save_interval)
      self$include_packages <- include_packages
      self$chunk_multiplier <- as.integer(chunk_multiplier)

      self$reset()
    },
    reset = function() {
      set.seed(self$set_seed)
      self$workers <- 1

      # create or clear log files
      if (self$progress_file) {
        cat(paste0(""), file = "workers_progress.log", append = FALSE)
        cat(paste0(""), file = "agents_progress.log", append = FALSE)
        cat(paste0(""), file = "parallel.log", append = FALSE)
        self$outfile <- "parallel.log"
      }

      # (re)create history data and meta data tables
      self$internal_history <- History$new()
      self$internal_history$set_meta_data("horizon",self$horizon)
      self$internal_history$set_meta_data("agents",self$agent_count)
      self$internal_history$set_meta_data("simulations",self$simulations)
      self$internal_history$set_meta_data("sim_start_time",format(Sys.time(), "%a %b %d %X %Y"))

      # unique policy name creation
      agent_name_list <- list()
      for (agent_index in 1L:self$agent_count) {
        current_agent_name <- self$agents[[agent_index]]$name
        agent_name_list <- c(agent_name_list,current_agent_name)
        current_agent_name_occurrences <-
          length(agent_name_list[agent_name_list == current_agent_name])
        if (current_agent_name_occurrences > 1) {
          self$agents[[agent_index]]$name <-
            paste0(current_agent_name,'.',current_agent_name_occurrences)
        }
        agent_name <-  self$agents[[agent_index]]$name
        bandit_name <- self$agents[[agent_index]]$bandit$class_name
        policy_name <- self$agents[[agent_index]]$policy$class_name
        self$internal_history$set_meta_data("bandit", bandit_name , group = "sim", agent_name = agent_name)
        self$internal_history$set_meta_data("policy", policy_name , group = "sim", agent_name = agent_name)
      }
    },
    run = function() {
      # set parallel or serial processing
      `%fun%` <- foreach::`%do%`

      # nocov start
      if (self$do_parallel) {
        self$register_parallel_backend()
        `%fun%` <- foreach::`%dopar%`
        # If Microsoft R, set MKL threads to 1
        if ("RevoUtilsMath" %in% rownames(installed.packages())) {
          RevoUtilsMath::setMKLthreads(1)
        }
      }
      # nocov end

      # create a list of all sims (sims*agents), to be divided into chunks
      index <- 1
      sims_and_agents_list <- vector("list", self$simulations*self$agent_count)
      for (sim_index in 1L:self$simulations) {
        for (agent_index in 1L:self$agent_count) {
          sims_and_agents_list[[index]] <-
            list(agent_index = agent_index, sim_index   = sim_index)
          index <- index + 1
        }
      }

      # copy variables used in parallel processing to local environment
      horizon                  <- self$horizon
      agent_count              <- self$agent_count
      save_context             <- self$save_context
      save_theta               <- self$save_theta
      progress_file            <- self$progress_file
      save_interval            <- self$save_interval
      log_interval             <- self$log_interval
      t_over_sims              <- self$t_over_sims
      set_seed                 <- self$set_seed
      agents                   <- self$agents
      include_packages         <- self$include_packages

      # calculate chunk size
      if (length(sims_and_agents_list) <= self$workers) {
        chunk_divider <- length(sims_and_agents_list)
      } else {
        chunk_divider <- self$workers * self$chunk_multiplier
      }
      # split sims vector into chuncks
      sa_iterator <- itertools::isplitVector(sims_and_agents_list, chunks = chunk_divider)
      # include packages that are used in parallel processes
      par_packages <- c(c("data.table","iterators","itertools"),include_packages)

      # some info messages
      message(paste("Simulation horizon:",horizon))
      message(paste("Number of simulations:",length(sims_and_agents_list)))
      message(paste("Number of batches:",chunk_divider))
      message("Starting main loop.")

      # start running the main simulation loop
      private$start_time <- Sys.time()
      foreach_results <- foreach::foreach(
        sims_agent_list = sa_iterator,
        i = iterators::icount(),
        .inorder = TRUE,
        .export = c("History","Formula"),
        .noexport = c("sims_and_agents_list","internal_history","sa_iterator"),
        .packages = par_packages
      ) %fun% {
        index <- 1L
        sim_agent_counter <- 0
        sim_agent_total <- length(sims_agent_list)

        # TODO: Can be done smarter and cleaner?
        multiplier <- 1
        for (sim_agent_index in sims_agent_list) {
          sim_agent <- agents[[sim_agent_index$agent_index]]
          if(isTRUE(sim_agent$bandit$arm_multiply))
            if(multiplier < sim_agent$bandit$k)
              multiplier <- sim_agent$bandit$k
        }
        allocate_space <- floor((horizon * sim_agent_total * multiplier) / save_interval) + sim_agent_total

        local_history <- History$new( allocate_space,
                                      save_context,
                                      save_theta)

        for (sim_agent_index in sims_agent_list) {
          sim_agent <- agents[[sim_agent_index$agent_index]]$clone(deep = TRUE)
          sim_agent$sim_index <- sim_agent_index$sim_index
          sim_agent$agent_index <- sim_agent_index$agent_index

          sim_agent_counter <- sim_agent_counter + 1
          if (isTRUE(progress_file)) {
            sim_agent$progress_file <- TRUE
            sim_agent$log_interval <- log_interval
            cat(paste0("[",format(Sys.time(), format = "%H:%M:%OS6"),"] ",
                       "        0 > init - ",sprintf("%-20s", sim_agent$name),
                       " worker ", i,
                       " at sim ", sim_agent_counter,
                       " of ", sim_agent_total,"\n"),
                file = "workers_progress.log", append = TRUE)
          }
          simulation_index <- sim_agent$sim_index
          agent_name <- sim_agent$name
          local_curent_seed <- simulation_index + set_seed * 42
          set.seed(local_curent_seed)
          sim_agent$bandit$post_initialization()
          if(isTRUE(sim_agent$bandit$arm_multiply))
            horizon_loop <- horizon * sim_agent$bandit$k
          else
            horizon_loop <- horizon
          set.seed(local_curent_seed + 1e+06)
          sim_agent$bandit$generate_bandit_data(n = horizon_loop)
          if (isTRUE(t_over_sims)) sim_agent$set_t(as.integer((simulation_index - 1L) * horizon_loop))
          step <- list()
          for (t in 1L:horizon_loop) {
            step <- sim_agent$do_step()
            if (!is.null(step[[3]]) && ((step[[5]] == 1) || (step[[5]] %% save_interval == 0))) {
              local_history$insert(
                index,                                         #index
                step[[5]],                                     #policy_t
                step[[1]][["k"]],                              #k
                step[[1]][["d"]],                              #d
                step[[2]],                                     #action
                step[[3]],                                     #reward
                agent_name,                                    #agentname
                simulation_index,                              #sim
                if (save_context) step[[1]][["X"]] else NA,    #context
                if (save_theta) step[[4]] else NA              #theta
              )
              index <- index + 1L
            }
          }
        }
        sim_agent$bandit$final()
        local_history$data[t!=0]
      }

      # bind all results
      foreach_results <- data.table::rbindlist(foreach_results)
      foreach_results[, agent := factor(agent)]
      self$internal_history$set_data_table(foreach_results[sim > 0 & t > 0], auto_stats = FALSE)
      rm(foreach_results)
      private$end_time <- Sys.time()
      gc()
      message("Finished main loop.")

      self$internal_history$set_meta_data("sim_end_time",format(Sys.time(), "%a %b %d %X %Y"))
      formatted_duration <- contextual::formatted_difftime(private$end_time - private$start_time)
      self$internal_history$set_meta_data("sim_total_duration", formatted_duration)
      message(paste0("Completed simulation in ",formatted_duration))

      # TODO: this should be optional, and maybe done at plotside?
      self$internal_history$truncate()

      start_time_stats <- Sys.time()
      message("Computing statistics.")
      # update statistics TODO: not always necessary, add option arg to class?
      self$internal_history$update_statistics()

      # set meta data and messages
      self$stop_parallel_backend()
      self$internal_history
    },
    register_parallel_backend = function() {
      # nocov start
      # setup parallel backend
      message("Setting up parallel backend.")
      nr_cores <- parallel::detectCores()
      if (nr_cores >= 3) self$workers <- nr_cores - 1
      if (!is.null(self$worker_max)) {
        if (self$workers > self$worker_max) self$workers <- self$worker_max
      }

      # make sure no leftover processes
      doParallel::stopImplicitCluster()


      if(!is.null(self$outfile)) {
        self$cl <- parallel::makeCluster(self$workers, useXDR = FALSE, type = "PSOCK",
                                         methods = FALSE, setup_timeout = 30, outfile = self$outfile)
      } else {
        self$cl <- parallel::makeCluster(self$workers, useXDR = FALSE, type = "PSOCK",
                                         methods = FALSE, setup_timeout = 30)
      }

      message(paste0("Cores available: ",nr_cores))
      message(paste0("Workers assigned: ",self$workers))
      doParallel::registerDoParallel(self$cl)
      # nocov end
    },
    stop_parallel_backend = function() {
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
  ),
  active = list(
    history = function(value) {
      if (missing(value)) {
        self$internal_history
      } else {
        warning("## history$data is read only", call. = FALSE)
      }
    }
  )
)


#' Simulator
#'
#' The entry point of any \pkg{contextual} simulation.
#'
#' A Simulator takes, at a minimum, one or more \code{\link{Agent}} instances, a horizon
#' (the length of an individual simulation, \emph{t} = \{1, \ldots, T\}) and the number of simulations
#' (How many times to repeat each simulation over \emph{t} = \{1, \ldots, T\}, with a new seed
#' on each repeat*).
#'
#' It then runs all simulations (in parallel by default), keeping a log of all \code{\link{Policy}} and
#' \code{\link{Bandit}} interactions in a \code{\link{History}} instance.
#'
#' &ast; Note: to be able to fairly evaluate and compare each agent's performance, and to make sure that
#' simulations are replicable, for each separate agent, seeds are set equally and deterministically for
#' each agent over all \code{horizon x simulations} time steps.
#'
#' ![](1simulator.jpeg "contextual diagram: simulator")
#'
#' @name Simulator
#' @aliases run simulator
#'
#' @section Usage:
#' \preformatted{
#' simulator <- Simulator$new(agents,
#'                            horizon = 100L,
#'                            simulations = 100L,
#'                            save_interval = 1,
#'                            save_context = FALSE,
#'                            save_theta = FALSE,
#'                            do_parallel = TRUE,
#'                            worker_max = NULL,
#'                            t_over_sims = FALSE,
#'                            set_seed = 0,
#'                            progress_file = FALSE,
#'                            include_packages = NULL)
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
#'   \item{\code{save_interval}}{
#'     \code{integer}. Write data to historyonly every \code{save_interval} time steps. Default is 1.
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
#'   }
#'   \item{\code{t_over_sims}}{
#'      \code{logical}. Of use to, among others, offline Bandits.
#'      If \code{t_over_sims} is set to \code{TRUE}, the current \code{Simulator}
#'      iterates over all rows in a data set for each repeated simulation.
#'      If \code{FALSE}, it splits the data into \code{simulations} parts,
#'      and a different subset of the data for each repeat of an agent's simulation.
#'   }
#'   \item{\code{set_seed}}{
#'      \code{integer}. Sets the seed of R's random number generator for the current \code{Simulator}.
#'   }
#'   \item{\code{progress_file}}{
#'       \code{logical}. If \code{TRUE}, \code{Simulator} writes \code{workers_progress.log},
#'       \code{agents_progress.log} and \code{parallel.log} files to the current working directory,
#'       allowing you to keep track of respectively \code{workers}, \code{agents},
#'       and potential errors when running a \code{Simulator} in parallel mode.
#'   }
#'   \item{\code{log_interval}}{
#'       \code{integer}. Sets the log write interval. Default every 1000 time steps.
#'   }
#'   \item{\code{include_packages}}{
#'       \code{List}. List of packages that (one of) the policies depend on. If a \code{Policy} requires an
#'       R package to be loaded, this option can be used to load that package on each of the workers.
#'       Ignored if \code{do_parallel} is \code{FALSE}.
#'   }
#'   \item{\code{chunk_multiplier}}{
#'      \code{integer} By default, simulations are equally divided over available workers, and every
#'      worker saves its simulation results to a local history file which is then aggregated.
#'      Depending on workload, network bandwith, memory size and other variables it can sometimes be useful to
#'      break these workloads into smaller chunks. This can be done by setting the chunk_multiplier to some
#'      integer value, where the number of chunks will total chunk_multiplier x number_of_workers.
#'   }
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
#'   \item{\code{history}}{
#'      Active binding, read access to Simulator's History instance.
#'   }
#'
#'  }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @examples
#' \dontrun{
#'
#'   policy    <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'   bandit    <- BasicBernoulliBandit$new(weights = c(0.6, 0.1, 0.1))
#'   agent     <- Agent$new(policy, bandit, name = "E.G.", sparse = 0.5)
#'
#'   history   <- Simulator$new(agents = agent,
#'                              horizon = 10,
#'                              simulations = 10)$run()
#'
#'   summary(history)
#'
#'   plot(history)
#'
#'   dt <- history$get_data_table()
#'
#'   df <- history$get_data_frame()
#'
#'   print(history$cumulative$E.G.$cum_regret_sd)
#'
#'   print(history$cumulative$E.G.$cum_regret)
#'
#' }
#'
NULL
