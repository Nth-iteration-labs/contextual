##################### Imports ###################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")
source("./dependent_observations/bandit_bernoulli.R")
source("./dependent_observations/policy_pooled_egreedy.R")
source("./dependent_observations/policy_pooled_ucb.R")
##################### Settings ##################

# first test: 13 hours, clearly inefficient implementation of policies,
# and no precaching bandit yet

# also, maybe save not *every* step, if not needed?

horizon     <- 10000
simulations <- 1000
subjects    <- list(5,10,50,100,500)
betas       <- list(c(1.5, 1.5),c(5, 5))

agents      <- list()
ptm         <- proc.time()

for (sn in subjects) {
  for (beta in betas) {

    sim_str     <- paste0("_",beta[1],"_",sn)

    bandit      <- BernoulliBandit$new( n_subjects = sn, arm_one_shape = beta, arm_two_shape = beta)

    agent       <- Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("PartialEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/PartialEG",sim_str,".RData"))

    agent       <- Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("UnpooledEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/UnpooledEG",sim_str,".RData"))

    agent       <- Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1, name = paste0("PooledEG",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/PooledEG",sim_str,".RData"))

    agent       <- Agent$new(PooledUCBPolicy$new(name = paste0("PooledUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/PooledUCB",sim_str,".RData"))

    agent       <- Agent$new(UnpooledUCBPolicy$new(n_subjects = sn, name = paste0("UnpooledUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/UnpooledUCB",sim_str,".RData"))

    agent       <- Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = sn, name = paste0("PartialUCB",sim_str)), bandit)
    history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
    history$save_data(paste0("data/PartialUCB",sim_str,".RData"))

  }
}

print(proc.time() - ptm)

# clear history
history  <- NULL

# create new history log object, load and append data
plot_log <- History$new()
plot_log$load_data("data/PartialEG_1.5_5.RData")
plot_log$load_data("data/UnpooledEG_1.5_5.RData")
plot_log$load_data("data/PooledEG_1.5_5.RData")
plot_log$load_data("data/PooledUCB_1.5_5.RData")
plot_log$load_data("data/UnpooledUCB_1.5_5.RData")
plot_log$load_data("data/PartialUCB_1.5_5.RData")

#plot(plot_log, type = "cumulative", regret = FALSE)
#plot(plot_log, type = "cumulative", rate = TRUE)
plot(plot_log, type = "cumulative", rate = FALSE)

# create new history log object, load and append data
plot_log <- History$new()
plot_log$load_data("data/PartialEG_1.5_5.RData")
plot_log$load_data("data/PartialEG_1.5_10.RData")
plot_log$load_data("data/PartialEG_1.5_50.RData")
plot_log$load_data("data/PartialEG_1.5_100.RData")
plot_log$load_data("data/PartialEG_1.5_500.RData")

plot(plot_log, type = "cumulative", rate = TRUE)
plot(plot_log, type = "cumulative", rate = FALSE)

