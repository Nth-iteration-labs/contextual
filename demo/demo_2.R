setwd("~/GitHub/contextual/demo")
source("dev.R")

# Here, we simulate a 4 armed bandit, k=4
# per arm, there are 2 features, with certain weights,
# always either on or off on a per arm basis
# for example, if arms are articles:
# article== sports 1|0 / article == science 1|0
# Also, there is a set of 6 features that are specific for time = t
# in an article situation, maybe, on time step = t,
# a visitor is male, language browser is english, etc
# This adds up to total of *8* context features d=8 every round

horizon             <- 500
simulations         <- 100

arm_weights         <- matrix(  c( 0.9, 0.1, 0.1, 0.1,
                                   0.1, 0.9, 0.1, 0.1), nrow = 2, ncol = 4, byrow = TRUE)

arm_masks           <- matrix(  c( 1,   0,   1,   1,
                                   0,   0,   1,   0),   nrow = 2, ncol = 4, byrow = TRUE)

context_weights     <- matrix(  c( 0.9, 0.1, 0.1, 0.1,
                                   0.1, 0.9, 0.1, 0.1,
                                   0.1, 0.9, 0.1, 0.1,
                                   0.9, 0.1, 0.1, 0.1,
                                   0.1, 0.1, 0.9, 0.9,
                                   0.1, 0.1, 0.9, 0.9), nrow = 6, ncol = 4, byrow = TRUE)


bandit             <- SyntheticBandit$new(arm_weights     = arm_weights,
                                          arm_masks       = arm_masks,
                                          context_weights = context_weights)

agents             <- list(Agent$new(LinUCBPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "cumulative")



h <- history$get_data_table()



