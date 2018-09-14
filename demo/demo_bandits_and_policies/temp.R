library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")



bandit             <- BasicBernoulliBandit$new(c(0.6, 0.1, 0.1))


agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit))


history            <- Simulator$new(agents = agents,
                                    horizon = 100,
                                    simulations = 10,
                                    do_parallel = FALSE,
                                    log_interval = 1,
                                    progress_file = TRUE)$run()

plot(history)

g <- history$get_data_table()
