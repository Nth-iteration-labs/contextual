##################### Imports ###################

#library(contextual)
library("rstan")
setwd("~/GitHub/contextual/demo/dependent_observations")
source("../dev.R")
source("./bandit_bernoulli.R")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##################### Settings ##################

horizon     <- 30
simulations <- 10
n_subjects  <- 100

bandit      <- BernoulliBandit$new( n_subjects = 50, arm_one_shape = c(1.5, 1.5), arm_two_shape = c(1.5, 1.5) )

##################### Thompson ###################

# import UnpooledThompson and PooledThompson policies
source("./policy_pooled_thompson.R")

agents      <- list(Agent$new(PartiallyPooledThompsonPolicy$new(n_subjects = n_subjects, name = "PartialT"), bandit),
                    Agent$new(PooledThompsonPolicy$new(name = "PooledT"), bandit),
                    Agent$new(UnpooledThompsonPolicy$new(n_subjects = n_subjects, name = "UnpooledT"), bandit))

ptm         <- proc.time()

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             do_parallel = FALSE)$run()

print(proc.time() - ptm)

plot(history, type = "cumulative", rate = FALSE)
