# AzureSimulator is a subclass of contextual's Simulator superclass
# It replaces doParallel with doAzureParallel.

# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")

library(doAzureParallel)
library(contextual)

AzureSimulator <- R6::R6Class(
  "AzureSimulator",
  portable = FALSE,
  inherit = Simulator,
  class = FALSE,
  private = list(rewards = NULL),
  public = list(
    register_parallel_backend = function() {
      # 1. Generate your credential and cluster configuration files.
      doAzureParallel::generateClusterConfig("cluster.json")
      doAzureParallel::generateCredentialsConfig("credentials.json")

      # 2. Fill out your credential config and cluster config files.

      # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
      doAzureParallel::setCredentials("credentials.json")

      # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned.
      super$cl <- doAzureParallel::makeCluster("cluster.json")

      # 5. Register the pool as your parallel backend
      doAzureParallel::registerDoAzureParallel(super$cl)

      # 6. Check that your parallel backend has been registered
      workers = foreach::getDoParWorkers()

      message(paste0("Workers: ", workers))
    },
    close = function() {
      if (super$do_parallel) {
        doAzureParallel::stopCluster(super$cl)
      }
    }
  )
)
