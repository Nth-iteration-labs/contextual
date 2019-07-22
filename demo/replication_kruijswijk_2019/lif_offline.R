library(contextual)
library(here)
setwd(here("demo","replication_kruijswijk_2019"))

source("./bandit_continuum_offon.R")
#source("./bandit_continuum_offon_kern.R")
source("./policy_tbl.R")
source("./policy_unifcont.R")
source("./policy_efirst_regression.R")

set.seed(1)

horizon            <- 1e+06
simulations        <- 10

continuous_arms    <- function(x) {
  c1 <- runif(1, 0.25, 0.75)
  #c2 <- runif(1, 0.25, 0.75)
  c2 <- 1
  -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
}

choice <- runif(horizon, min=0, max=1)
reward <- continuous_arms(choice)
offline_data <- data.frame(choice, reward)

int_time           <- 50
amplitude          <- 0.05
learn_rate         <- 1
omega              <- 1#2*pi/int_time
x0_start           <- runif(1)#2.0


################# Different policies ###########
#bandit             <- OnlineOfflineContinuumBandit$new(delta = 0.1, horizon = horizon)
# bandit             <- OnlineOfflineContinuumBanditKernel$new(horizon = horizon)
#
# agents              <- list(#Agent$new(UniformRandomContinuousPolicy$new(), bandit),
#                             Agent$new(ThompsonBayesianLinearPolicy$new(), bandit))#,
#                             #Agent$new(LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start), bandit))#,
#                             #Agent$new(EFirstRegressionPolicy$new(epsilon = 100), bandit))

################ Different bandits offline #############

bandits <- list(ContinuumBandit$new(FUN = continuous_arms),
                OnlineOfflineContinuumBandit$new(delta = 0.5, horizon = horizon/0.5),
                OnlineOfflineContinuumBandit$new(delta = 0.2, horizon = horizon/0.2),
                OnlineOfflineContinuumBandit$new(delta = 0.1, horizon = horizon/0.1),
                OnlineOfflineContinuumBandit$new(delta = 0.05, horizon = horizon),
                OnlineOfflineContinuumBandit$new(delta = 0.01, horizon = horizon))

policy <- LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start)

agents <- list(Agent$new(policy, bandits[[1]]),
               Agent$new(policy, bandits[[2]]),
               Agent$new(policy, bandits[[3]]),
               Agent$new(policy, bandits[[4]]),
               Agent$new(policy, bandits[[5]]),
               Agent$new(policy, bandits[[6]]))

history            <- Simulator$new(agents      = agents,
                                    horizon     = horizon,
                                    simulations = simulations,
                                    do_parallel = FALSE)$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, disp = 'ci', legend_position = 'bottomright', ylim = c(0.5,1), trunc_over_agents = FALSE, trunc_per_agent = FALSE)
