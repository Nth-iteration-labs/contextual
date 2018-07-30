# AzureSimulator is a copy of contextual's Simulator class
# with some minor changes that enable it
# to replace doParallel with doAzureParallel.

# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")

library(doAzureParallel)
library(contextual)

#' @export
AzureSimulator <- R6::R6Class(
  "AzureSimulator",
  portable = FALSE,
  class = FALSE,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agents_length = NULL,
    horizon = NULL,
    simulations = NULL,
    worker_max = NULL,
    history = NULL,
    save_context = NULL,
    save_theta = NULL,
    do_parallel = NULL,
    sims_per_agent_list = NULL,
    t_over_sims = NULL,
    set_seed = NULL,
    progress_file = NULL,
    include_packages = NULL,
    cl = NULL,
    reindex_t = NULL,
    chunks = NULL,
    initialize = function(agents,
                          horizon = 100L,
                          simulations = 100L,
                          chunks = 8,
                          save_context = FALSE,
                          save_theta = FALSE,
                          do_parallel = TRUE,
                          worker_max = NULL,
                          t_over_sims = FALSE,
                          set_seed = 0,
                          progress_file = TRUE,
                          include_packages = NULL,
                          reindex_t = FALSE) {
      self$reindex_t <- reindex_t
      self$horizon <- horizon
      self$chunks <- chunks
      self$simulations <- simulations
      self$save_theta <- save_theta
      self$save_context <- save_context
      if (!is.list(agents)) agents <- list(agents)
      self$agents <- agents
      self$agents_length <- length(agents)
      self$worker_max <- worker_max
      self$do_parallel <- do_parallel
      self$t_over_sims <- t_over_sims
      self$set_seed <- set_seed
      self$progress_file <- progress_file
      self$include_packages <- include_packages
      self$reset()
    },
    reset = function() {
      set.seed(self$set_seed)

      # create empty progress.log file
      if (self$progress_file) cat(paste0(""), file = "progress.log", append = FALSE)
      # clear doparallel.log
      if (self$progress_file) cat(paste0(""), file = "doparallel.log", append = FALSE)

      # (re)create history's data.table
      self$history <- History$new(self$horizon * self$agents_length * self$simulations)
      self$sims_per_agent_list <- matrix(list(), self$simulations, self$agents_length)
      # unique policy names through appending sequence numbers to duplicates
      policy_name_list <- list()
      for (agent_index in 1L:self$agents_length) {

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
        for (agent_index in 1L:self$agents_length) {
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

        ##################################### additional doAzureParallel configuration ########################

        # 1. Generate your credential and cluster configuration files.
        doAzureParallel::generateClusterConfig("cluster.json")
        doAzureParallel::generateCredentialsConfig("credentials.json")

        # 2. Fill out your credential config and cluster config files.
        # Enter your Azure Batch Account & Azure Storage keys/account-info into your credential
        # config ("credentials.json") and configure your cluster in your cluster config ("cluster.json")

        # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
        doAzureParallel::setCredentials("credentials.json")

        # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
        self$cl <- doAzureParallel::makeCluster("cluster.json")

        # 5. Register the pool as your parallel backend
        doAzureParallel::registerDoAzureParallel(self$cl)

        # 6. Check that your parallel backend has been registered
        workers = foreach::getDoParWorkers()

        message(paste0("Workers: ", workers))
        message(paste0("Chunks: ", self$chunks))

        `%fun%` <- foreach::`%dopar%`
        message("Postworkercreation")
      }
      # copy relevant variables to local environment
      horizon <- self$horizon
      sims_per_agent_list <- self$sims_per_agent_list
      agents_length <- self$agents_length
      save_context <- self$save_context
      save_theta <- self$save_theta
      reindex_t <- self$reindex_t
      progress_file <- self$progress_file
      t_over_sims <- self$t_over_sims
      set_seed <- self$set_seed
      chunks <- self$chunks
      # calculate chunk size
      sa_iterator <- itertools::isplitRows(sims_per_agent_list, chunks = chunks)
      # include packages that are used in parallel processes
      par_packages <- c(c("data.table","itertools"),include_packages)
      # running the main simulation loop
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
        local_history <- History$new( horizon * agents_length * sim_agent_total, save_context, save_theta)
        for (sim_agent in sims_agents) {
          sim_agent_counter <- sim_agent_counter + 1
          if (progress_file) {
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
          sim_agent$bandit$post_initialization()
          if (sim_agent$bandit$precaching ) {
            sim_agent$bandit$generate_bandit_data(n = horizon)
          }
          if (t_over_sims) sim_agent$set_t(as.integer((simulation_index - 1L) * horizon))
          for (t in 1L:horizon) {
            step <- sim_agent$do_step()
            if (!is.null(step$reward)) {
              local_history$insert(
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
      self$history$set_data_table(foreach_results)
      if (reindex_t) self$history$reindex_t()
      self$history
    },
    finalize = function() {
      if (self$do_parallel) {
        doAzureParallel::stopCluster(self$cl)
        # Making sure that we are closing all processes that were
        # spawned but (potentially) not terminated by the foreach loop.
        closeAllConnections()
      }
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
#'                            t_over_sims = FALSE,
#'                            set_seed = 0,
#'                            progress_file = TRUE,
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
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{BasicContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
