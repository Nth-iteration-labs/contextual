# RedisSimulator is a subclass of Simulator
# substituting doParallel with doRedis.

# Before running the example, follow instructions at:
#
# https://github.com/bwlewis/doRedis
#
# Then open one or more R sessions that will act as back-end worker processes.
# Run the following in each session:
#
# require('doRedis')
# redisWorker('jobs')

library(contextual)
library(foreach)
library(doRedis)

RedisSimulator <- R6::R6Class(
  inherit = Simulator,
  public = list(
    register_parallel_backend = function() {
      options('redis:num'=TRUE)
      doRedis::registerDoRedis('jobs')
      workers = foreach::getDoParWorkers()
    },
    stop_parallel_backend = function() {
      try({
        doRedis::removeQueue('jobs')
      })
    }
  )
)
