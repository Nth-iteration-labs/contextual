library(contextual)
library(here)

setwd(here::here("demo","alt_par_backend_examples","azure"))

source("simulator_azure.R")

#devtools::install_github("Azure/rAzureBatch")
#devtools::install_github("Azure/doAzureParallel")

## follow setup and install of doAzureParallel
## at https://github.com/Azure/doAzureParallel

## sample credentials in the same directory as this file
## add your credentials and save to the current directory

horizon       <- 1000L
simulations   <- 4L

bandit        <- ContextualLinearBandit$new(k = 5, d = 5)

agents        <-list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                     Agent$new(ContextualLogitBTSPolicy$new(10), bandit),
                     Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0), bandit))

simulation     <- AzureSimulator$new(agents, horizon, simulations)

history        <- simulation$run()

plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")
