library(contextual)
library(here)
#library(rstan)
setwd(here("demo","replication_kruijswijk_2018"))

source("./bandit_bernoulli.R")

##################### Settings ###############################################################################

horizon     <- 100000
simulations <- 10
n_subjects  <- 50

bandit      <- BernoulliBandit$new(n_users = n_subjects, arm_one_shape = c(1.5, 1.5), arm_two_shape = c(1.5, 1.5))

##################### eGreedy ################################################################################

# import PartiallyPooledEgreedy, UnpooledEgreedy and PooledEgreedy policies
source("policy_pooled_egreedy.R")

agents      <- list(Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1), bandit),
                    Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyBBPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyPooledEgreedyPolicyNew$new(epsilon = 0.1, n_subjects = n_subjects), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations)$run()

plot(history, type = "cumulative", rate = FALSE)

##################### Thompson ###################

# import UnpooledThompson and PooledThompson policies

source("policy_pooled_thompson.R")

message("Stan Modeling phase")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
model  <- rstan::stan_model(file = "beta_binom_hier_model.stan", save_dso = TRUE, auto_write = TRUE )

agents      <- list(Agent$new(PooledThompsonPolicy$new(), bandit),
                    Agent$new(UnpooledThompsonPolicy$new(n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyPooledThompsonPolicy$new(n_subjects = n_subjects,
                                                                stan_model = model,
                                                                warm_up = 10,
                                                                iter = 20), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             include_packages = "rstan",
                             simulations = simulations)$run()

plot(history, type = "cumulative", rate = FALSE)

##################### UCB ####################################################################################

# import PartiallyPooledUCB, UnpooledUCB and PooledUCB policies

source("policy_pooled_ucb.R")

agents      <- list(Agent$new(PooledUCBPolicy$new(), bandit),
                    Agent$new(UnpooledUCBPolicy$new(n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyPooledUCBPolicyNew$new(n_subjects = n_subjects), bandit),
                    Agent$new(PartiallyBBPooledUCBPolicy$new(n_subjects = n_subjects), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations)$run()

plot(history, type = "cumulative", rate = FALSE)


