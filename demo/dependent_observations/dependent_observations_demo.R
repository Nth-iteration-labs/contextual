##################### Imports ###################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

source("./dependent_observations/bandit_bernoulli.R")

##################### Settings ##################

horizon     <- 3000
simulations <- 10
n_subjects  <- 100
lambda      <- 4

bandit      <- BernoulliBandit$new( n_subjects = n_subjects,
                                    arm_one_shape = c(1.5, 1.5),
                                    arm_two_shape = c(1.5, 1.5),
                                    poisson_user_sampling = TRUE,
                                    lambda = 2 )

##################### eGreedy ###################

# import PartiallyPooledEgreedy, UnpooledEgreedy and PooledEgreedy policies
source("./dependent_observations/policy_pooled_egreedy.R")

agents      <- list(Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_subjects, name = "PartialEG"), bandit),
                    Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_subjects, name = "UnpooledEG"), bandit),
                    Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1, name = "PooledEG"), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             save_theta = FALSE,
                             do_parallel = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE)
plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "cumulative", rate = FALSE)

##################### Thompson ###################

# import UnpooledThompson and PooledThompson policies
source("./dependent_observations/policy_pooled_thompson.R")

agents      <- list(Agent$new(PooledThompsonPolicy$new(name = "PooledT"), bandit),
                    Agent$new(UnpooledThompsonPolicy$new(n_subjects = n_subjects, name = "UnpooledT"), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             save_theta = FALSE,
                             do_parallel = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE)
plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "cumulative", rate = FALSE)

##################### UCB ########################

# import PartiallyPooledUCB, UnpooledUCB and PooledUCB policies
source("./dependent_observations/policy_pooled_ucb.R")

agents      <- list(Agent$new(PooledUCBPolicy$new(name = "PooledUCB"), bandit),
                    Agent$new(UnpooledUCBPolicy$new(n_subjects = n_subjects, name = "UnpooledUCB"), bandit),
                    Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = n_subjects, name = "PartialUCB"), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             save_theta = FALSE,
                             do_parallel = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE)
plot(history, type = "cumulative", rate = TRUE)
plot(history, type = "cumulative", rate = FALSE)

