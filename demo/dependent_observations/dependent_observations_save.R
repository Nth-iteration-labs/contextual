##################### Imports ####################

#library(contextual)

setwd("~/GitHub/contextual/demo/dependent_observations")
source("../dev.R")
source("./bandit_bernoulli.R")
source("./policy_pooled_egreedy.R")
source("./policy_pooled_ucb.R")
source("./policy_pooled_thompson.R")

##################### Settings ###################

horizon        <- 10
simulations    <- 10
betas          <- list(c(1.5, 1.5),c(5, 5))
subjects       <- list(5,10,50,100,500,1000)
do_poisson     <- list(FALSE,TRUE)
poisson_lambda <- 7.58
data_dir       <- "D:/do_data_tmp/"

##################### Create Stan Model ##########

if (!require(rstan)) {
  install.packages("rstan")
  library("rstan")
}

message("Stan Modeling phase")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model  <- rstan::stan_model(file = "beta_binom_hier_model.stan", save_dso = TRUE, auto_write = TRUE )

##################### Simulate ###################

# total number of simulations: 9 x 6 x 2 x 2 = 216

ptm         <- proc.time()
agents      <- list()

for (dp in do_poisson) {
  for (sn in subjects)   {
    for (beta in betas)    {

      sim_str     <- paste0("_b",beta[1],"_s",sn,"_p",as.numeric(dp))

      ##################### Bandit ####################

      bandit      <- BernoulliBandit$new( n_subjects = sn,
                                          arm_one_shape = beta,
                                          arm_two_shape = beta,
                                          subject_poisson_sampling = dp,
                                          lambda = poisson_lambda  )

      ##################### Egreedy Policies #################

      agent       <- Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("PartialEG",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"PartialEG",sim_str,".RData"))

      agent       <- Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = sn, name = paste0("UnpooledEG",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"UnpooledEG",sim_str,".RData"))

      agent       <- Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1, name = paste0("PooledEG",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"PooledEG",sim_str,".RData"))

      ##################### UCB Policies #####################

      agent       <- Agent$new(PooledUCBPolicy$new(name = paste0("PooledUCB",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"PooledUCB",sim_str,".RData"))

      agent       <- Agent$new(UnpooledUCBPolicy$new(n_subjects = sn, name = paste0("UnpooledUCB",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"UnpooledUCB",sim_str,".RData"))

      agent       <- Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = sn, name = paste0("PartialUCB",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"PartialUCB",sim_str,".RData"))

      ##################### Thompson Sampling Policies ########

      agent       <- Agent$new(PooledThompsonPolicy$new(name = paste0("PooledThompson",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"PooledThompson",sim_str,".RData"))

      agent       <- Agent$new(UnpooledThompsonPolicy$new(n_subjects = sn, name = paste0("UnpooledThompson",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations)$run()
      history$save_data(paste0(data_dir,"UnpooledThompson",sim_str,".RData"))

      agent       <- Agent$new(PartiallyPooledThompsonPolicy$new(n_subjects = sn, stan_model = model, warm_up = 10, iter = 20, name = paste0("PartialThompson",sim_str)), bandit)
      history     <- Simulator$new(agents = agent, horizon = horizon, simulations = simulations, include_packages = "rstan")$run()
      history$save_data(paste0(data_dir,"PartialThompson",sim_str,".RData"))

    }
  }
}

print(proc.time() - ptm)

# clear history
history  <- NULL
