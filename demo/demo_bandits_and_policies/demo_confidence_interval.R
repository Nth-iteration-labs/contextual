library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon            <- 1000L
simulations        <- 2000L

weights            <- matrix(  c( 0.4, 0.2,
                                  0.4, 0.6),    nrow = 2, ncol = 2, byrow = TRUE )

policy             <- ContextualThompsonSamplingPolicy$new()
bandit             <- SyntheticBandit$new(weights = weights, precaching = TRUE, random_one_feature = TRUE)
agent_contextual   <- Agent$new(policy, bandit, name="contextual")

weights            <- matrix(  c( 0.4, 0.4, 0.4, 0.4),  nrow = 2, ncol = 2, byrow = TRUE )

policy2            <- ContextualThompsonSamplingPolicy$new()
bandit2            <- SyntheticBandit$new(weights = weights, precaching = TRUE, random_one_feature = TRUE)
agent_ncontextual  <- Agent$new(policy2, bandit2, name="non-contextual")


history            <- Simulator$new(list(agent_contextual,agent_ncontextual),
                                    horizon, simulations, save_theta = TRUE, save_context = TRUE,
                                    do_parallel = TRUE)$run()

#plot(history, type = "average", ci = TRUE,  write_log = 50)
plot(history, type = "cumulative", ci = TRUE, rate = TRUE, regret = FALSE)
