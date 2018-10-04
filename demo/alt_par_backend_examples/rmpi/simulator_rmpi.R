# MPISimulator is a subclass of Simulator
# substituting doParallel with doMPI.

library(contextual)
library(foreach)
library(Rmpi)
library(doMPI)

MPISimulator <- R6::R6Class(
  inherit = Simulator,
  public = list(
    register_parallel_backend = function() {
      super$cl <- doMPI::startMPIcluster()
      doMPI::registerDoMPI(super$cl)
      workers = foreach::getDoParWorkers()
      message(paste0("MPI workers: ", workers))
    },
    stop_parallel_backend = function() {
      try({
        doMPI::closeCluster(super$cl)
      })
    }
  )
)
