##################### Imports ###################

#library(contextual)
library("rstan")
setwd("~/GitHub/contextual/demo/dependent_observations")
source("../dev.R")
source("./bandit_bernoulli.R")

##################### Settings ##################

horizon     <- 3000
simulations <- 100
n_subjects  <- 50

bandit      <- BernoulliBandit$new( n_subjects = n_subjects, arm_one_shape = c(1.5, 1.5), arm_two_shape = c(1.5, 1.5) )

##################### Thompson ###################

# import UnpooledThompson and PooledThompson policies
source("./policy_pooled_thompson.R")

# do Stan modeling
message("Start Stan Modeling phase")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model  <- rstan::stan_model(file = "beta_binom_hier_model.stan", save_dso = TRUE, auto_write = TRUE )
message("End Stan modeling phase")

agents      <- list(Agent$new(PartiallyPooledThompsonPolicy$new(n_subjects = n_subjects,
                                                                stan_model = model,
                                                                warm_up = 10,
                                                                iter = 20,
                                                                name = "PartialT"), bandit),
                    Agent$new(PooledThompsonPolicy$new(name = "PooledT"), bandit),
                    Agent$new(UnpooledThompsonPolicy$new(n_subjects = n_subjects, name = "UnpooledT"), bandit))

ptm         <- proc.time()

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             do_parallel = FALSE,
                             include_packages = "rstan")$run()

print(proc.time() - ptm)

plot(history, type = "cumulative", rate = FALSE)
