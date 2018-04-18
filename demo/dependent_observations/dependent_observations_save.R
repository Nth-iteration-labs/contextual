##################### Imports ###################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")
source("./dependent_observations/bandit_bernoulli.R")
source("./dependent_observations/policy_pooled_egreedy.R")
source("./dependent_observations/policy_pooled_ucb.R")

##################### Settings ##################

horizon     <- 10000
simulations <- 1000
subjects    <- list(5,10,50,100,500)
betas       <- list(c(1.5, 1.5),c(5, 5))

data_dir     <- "D:/do_data_tmp/"

##################### Simulate ###################

agents      <- list()
ptm         <- proc.time()

for (sn in subjects) {
  for (beta in betas) {

    sim_str     <- paste0("_",beta[1],"_",sn)

    bandit      <- BernoulliBandit$new( n_subjects = sn, arm_one_shape = beta, arm_two_shape = beta)

    agent       <- Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("PartialEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"PartialEG",sim_str,".RData"))

    agent       <- Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("UnpooledEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"UnpooledEG",sim_str,".RData"))

    agent       <- Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1, name = paste0("PooledEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"PooledEG",sim_str,".RData"))

    agent       <- Agent$new(PooledUCBPolicy$new(name = paste0("PooledUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"PooledUCB",sim_str,".RData"))

    agent       <- Agent$new(UnpooledUCBPolicy$new(n_subjects = sn, name = paste0("UnpooledUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"UnpooledUCB",sim_str,".RData"))

    agent       <- Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = sn, name = paste0("PartialUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0(data_dir,"PartialUCB",sim_str,".RData"))

  }
}

print(proc.time() - ptm)

# clear history
history  <- NULL


