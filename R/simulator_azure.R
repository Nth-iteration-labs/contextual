#' @import foreach
#' @import doAzureParallel
#' @export
SimulatorAzure <- R6::R6Class(
  "SimulatorAzure",
  portable = FALSE,
  class = FALSE,
  private = list(rewards = NULL),
  public = list(
    agents = NULL,
    agent_n = NULL,
    horizon = NULL,
    simulations = NULL,
    history = NULL,

    initialize = function(agents,horizon = 100L, simulations = 100L) {
      self$horizon <- horizon
      self$simulations <- simulations
      if (!is.list(agents)) agents = list(agents)
      self$agents <- agents
      self$agent_n <- length(agents)
      self$history <- History$new(self$horizon * self$agent_n * self$simulations)
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n) {
        self$agents[[a]]$reset()
        if (self$agents[[a]]$bandit$is_precaching)
          self$agents[[a]]$generate_cache(self$horizon*self$simulations)
      }
    },
    run = function() {
      agent <-  matrix(list(), self$agent_n, self$simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a, s]  <- list(self$agents[[a]]$clone(deep = FALSE))

        }
      }

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a,s] <- list(self$agents[[a]]$clone(deep = FALSE))
        }
      }

      # 1. Generate your credential and cluster configuration files.
      doAzureParallel::generateClusterConfig("cluster.json")
      doAzureParallel::generateCredentialsConfig("credentials.json")
      doAzureParallel::setVerbose(TRUE)

      # 2. Fill out your credential config and cluster config files.
      # Enter your Azure Batch Account & Azure Storage keys/account-info into your credential config ("credentials.json") and configure your cluster in your cluster config ("cluster.json")

      # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
      doAzureParallel::setCredentials("credentials.json")

      # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
      cluster <- doAzureParallel::makeCluster("cluster.json")

      # 5. Register the pool as your parallel backend
      doAzureParallel::registerDoAzureParallel(cluster)

      # 6. Check that your parallel backend has been registered
      workers <- foreach::getDoParWorkers()

      `%do%` <- foreach::`%do%`
      `%dopar%` <- foreach::`%dopar%`

      opt <- list(enableMerge = FALSE)

      parallel_results <- foreach::foreach(
        s = 1L:self$simulations,
        .inorder = FALSE,
        .export = c("agent","self"),
        .packages = c("data.table"),
        .options.azure = opt
      ) %dopar% {
        counter <- 1L
        for (a in 1L:self$agent_n) {
          for (t in 1L:self$horizon) {

            agent_counter = as.integer(s + ((t - 1L) * self$simulations))

            context <- agent[[a,s]]$bandit_get_context(agent_counter)            # observe the bandit in its context
            action  <- agent[[a,s]]$policy_get_decision(agent_counter)           # use policy to decide which choice to make (which arm to pick)
            reward  <- agent[[a,s]]$bandit_get_reward(agent_counter)             # observe the resonse of the bandit in this context


            if (!is.null(reward)) {
              theta <- agent[[a,s]]$policy_set_reward(agent_counter)            # adjust the policy, update theta


              self$history$save_agent(counter,                                  # save the results to the history log
                                      t,
                                      action,
                                      reward,
                                      context$X,
                                      agent[[a,s]]$policy$name,
                                      s,
                                      theta)

              counter <- counter + 1L
            }
          }
        }
        dth <- self$history$get_data_table()
        dth[sim != 0]
      }
      parallel_results <- data.table::rbindlist(parallel_results)
      self$history$set_data_table(parallel_results)

      doAzureParallel::stopCluster(cluster)

      parallel_results
    }
  )
)

#' External SimulatorAzure
#'
#' SimulatorAzure intro
#'
#' @section Usage:
#' \preformatted{b <- SimulatorAzure$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{SimulatorAzure} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new SimulatorAzure, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name SimulatorAzure
#' @examples
#'\dontrun{}
#'
NULL
