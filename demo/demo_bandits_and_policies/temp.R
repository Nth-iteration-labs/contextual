library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")



bandit      <- ContextualLogitBandit$new(k = 5, d = 5, intercept = TRUE)
#expect_identical(typeof(bandit), "environment")

horizon       <- 10L
simulations   <- 10L

LinUCBGeneralPolicy

policy        <- ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5)
#expect_identical(typeof(policy), "environment")

agent         <- Agent$new(policy, bandit)
#expect_identical(typeof(agent), "environment")

simulation    <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
history       <- simulation$run()

print(history$cumulative$ContextualThompsonSampling$cum_regret)
