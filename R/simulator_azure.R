
#' @import foreach
#' @import doAzureParallel
#' @export
SimulatorAzure <- R6::R6Class(
  "SimulatorAzure",
  portable = FALSE,
  private = list(rewards = NULL),
  public = list(
    agent_list = NULL,
    agent_n = NULL,
    horizon = 100L,
    simulations = 1L,
    history = NULL,

    initialize = function(agent_list) {
      self$history <- History$new()
      self$agent_list <- agent_list
      self$agent_n <- length(agent_list)
      self$reset()
    },
    reset = function() {
      for (a in 1L:self$agent_n)
        self$agent_list[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {

      self$horizon <- horizon
      self$simulations <- simulations
      agent <- matrix(list(), self$agent_n, simulations)

      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent[a,s] <- list(self$agent_list[[a]]$clone(deep = FALSE))
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

      n <- self$horizon  * self$agent_n
      self$history$reset(n)

      `%do%` <- foreach::`%do%`
      `%dopar%` <- foreach::`%dopar%`

      opt <- list(enableMerge = FALSE)

      parallel_results <- foreach::foreach(
        s = 1L:self$simulations,
        .inorder = FALSE,
        .export = 'agent',
        .packages = c("data.table"),
        .options.azure = opt
      ) %dopar% {
        counter <- 1L
        for (a in 1L:self$agent_n) {
          for (t in 1L:self$horizon) {

                      agent[[a,s]]$observe_bandit(t)
            action <- agent[[a,s]]$get_policy_decision(t)
            reward <- agent[[a,s]]$get_bandit_reward(t)
                      agent[[a,s]]$adjust_policy(t)

            self$history$save_agent(counter,t,action,reward,agent[[a,s]]$policy$name,s)

            counter <- counter + 1L
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
