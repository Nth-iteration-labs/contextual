library(contextual)
library(here)
setwd(here("demo","replication_kruijswijk_2019"))

source("./bandit_continuum_offon.R")
source("./policy_tbl.R")
source("./policy_unifcont.R")
source("./policy_efirst_regression.R")

set.seed(100)



horizon            <- 10000
simulations        <- 10

continuous_arms    <- function(x, c1 = 0.25, c2 = 0.75) {
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



bandit             <- OnlineOfflineContinuumBandit$new(delta = 0.1, horizon = horizon)


agents              <- list(Agent$new(UniformRandomContinuousPolicy$new(), bandit),
                            Agent$new(ThompsonBayesianLinearPolicy$new(), bandit),
                            Agent$new(LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start), bandit),
                            Agent$new(EFirstRegressionPolicy$new(epsilon = 100), bandit))


history            <- Simulator$new(agents      = agents,
                                    horizon     = horizon,
                                    simulations = simulations,
                                    do_parallel = TRUE)$run()

plot(history, type = "cumulative", regret = TRUE,
     rate = FALSE, disp = 'ci', trunc_over_agents = FALSE, trunc_per_agent = FALSE)
