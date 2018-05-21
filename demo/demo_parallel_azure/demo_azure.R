setwd("~/GitHub/contextual/demo/demo_parallel_azure")
source("simulator_azure.R")
library(contextual)

## follow setup and install of doAzureParallel
## at https://github.com/Azure/doAzureParallel

## sample credentials in the same directory as this file
## add your credentials and save to the current directory

horizon            <- 1000L
simulations        <- 1000L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.5, 0.1,
                                  0.1, 0.1, 0.1), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = context_weights, precaching = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )

# the parameter chunks = 10 will split 1000 simulations into 10 batches of 100 simulations
# where each batch will be processed by one Azure compute node.

simulation         <- AzureSimulator$new(agents, horizon, simulations,
                                         chunks = 10, do_parallel = TRUE)
history            <- simulation$run()

plot(history, type = "cumulative")
