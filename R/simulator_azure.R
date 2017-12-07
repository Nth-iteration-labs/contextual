#' @import foreach
#' @import doAzureParallel
#' @import doRNG
#' @export
SimulatorAzure <- R6::R6Class(
  "SimulatorAzure",

  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  private = list(rewards = NULL),

  public = list(
    agent_list = NULL,
    agent_n = NULL,
    horizon = 100L,
    simulations = 1L,
    history = NULL,

    initialize = function(agent_list) {
      self$history = History$new()
      self$agent_list = agent_list
      self$agent_n = length(agent_list)
      generateClusterConfig("cluster.json")
      generateCredentialsConfig("credentials.json")
      self$reset()
    },
    reset = function() {
      for (a in 1L:agent_n)
        agent_list[[a]]$reset()
    },
    run = function(horizon = 100L,
                   simulations = 100L) {
      self$horizon = horizon
      self$simulations = simulations
      agent_instance =  matrix(list(), agent_n, simulations)
      bandit_instance = matrix(list(), agent_n, simulations)
      for (s in 1L:self$simulations) {
        for (a in 1L:self$agent_n) {
          agent_instance[a, s]  = list(self$agent_list[[a]]$clone())            #deep? in init?
          bandit_instance[a, s] = list(self$agent_list[[a]]$bandit$clone())
        }
      }

      # 1. Generate your credential and cluster configuration files.
      doAzureParallel::generateClusterConfig("cluster.json")
      doAzureParallel::generateCredentialsConfig("credentials.json")

      # 2. Fill out your credential config and cluster config files.
      # Enter your Azure Batch Account & Azure Storage keys/account-info into your credential config ("credentials.json") and configure your cluster in your cluster config ("cluster.json")

      # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
      doAzureParallel::setCredentials("credentials.json")

      # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
      cluster <- doAzureParallel::makeCluster("cluster.json")

      # 5. Register the pool as your parallel backend
      doAzureParallel::registerDoAzureParallel(cluster)

      # 6. Check that your parallel backend has been registered
      workers = foreach::getDoParWorkers()


      n = as.integer(ceiling(self$horizon / workers *
                               self$agent_n *
                               self$simulations))
      self$history$reset(n)
      `%dopar%` <- foreach::`%dopar%`
      parallel_results = foreach::foreach(
        t = 1L:self$horizon,
        .inorder = TRUE,
        .packages = c("data.table")
      ) %dopar% {
        parallel_counter <- 1L
        for (a in 1L:self$agent_n) {
          for (s in 1L:self$simulations) {
            context  = bandit_instance[[a, s]]$get_context()
            action   = agent_instance[[a, s]]$get_action(context)
            reward   = bandit_instance[[a, s]]$get_reward(action)
            agent_instance[[a, s]]$set_reward(reward, context)

            self$history$save_step(parallel_counter,
                                   t,
                                   s,
                                   action,
                                   reward,
                                   agent_instance[[a, s]]$policy$name)

            parallel_counter <- parallel_counter + 1L
          }
        }
        self$history$get_data_table()
      }
      parallel_results = data.table::rbindlist(parallel_results)[sim != 0]
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
