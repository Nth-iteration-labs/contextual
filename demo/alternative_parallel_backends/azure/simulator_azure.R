# AzureSimulator is a subclass of Simulator
# substituting doParallel with doAzureParallel.

# devtools::install_github("Azure/rAzureBatch")
# devtools::install_github("Azure/doAzureParallel")

library(contextual)
library(foreach)
library(doAzureParallel)
library(here)

setwd(here::here("demo","alternative_parallel_backends","azure"))

AzureSimulator <- R6::R6Class(
  portable = FALSE,
  inherit = Simulator,
  class = FALSE,
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

      message(paste0("Azure workers: ", workers))
    },
    stop_parallel_backend = function() {
      try({
        doAzureParallel::stopCluster(super$cl)
      })
    }
  )
)
