library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")




weight_per_arm     <- c(0.9, 0.1, 0.1)
horizon            <- 10
simulations        <- 10

bandit             <- BasicBernoulliBandit$new(weight_per_arm)

agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(EpsilonFirstPolicy$new(4), bandit)
                           #Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           #Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           #Agent$new(Exp3Policy$new(0.1), bandit),
                           #Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                           #Agent$new(UCB1Policy$new(), bandit),
                           #Agent$new(SoftmaxPolicy$new(0.1), bandit),
                           #Agent$new(SimpleBTSPolicy$new(), bandit)
                           )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()
