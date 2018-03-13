setwd("~/GitHub/contextual/demo")
source("dev.R")


# Synopsis: we have feature matrix X, that can contain arm features (always there for an arm)
# and context features (changing each time step t, yet at t the same for all arms)
# This matrix X has d total features, split into d_arm arm features, and d_context context features.
# Also, sub-feature matrices are called respectively X_a and X_c, and vectors x_a and x_c

# For exampe, here, we simulate a 4 armed bandit, k=4.
# Per arm, there are 4 arm-based features d_arm,
# for example, if arms are articles:
# article is sports 1|0 / science 1|0 / culture 1|0 /  economics 1|0
# Also, there is a set of d_ccntext 2 features that are specific
# for a particular time step, for example,
# a user visiting is female 1|0 / language browser is english 1|0
# This adds up to total of *6* features d=6 every round

horizon            <- 1000
simulations        <- 50

arm_weights        <- matrix(  c( 0.3, 0.8, 0.4, 0.8,
                                  0.3, 0.2, 0.2, 0.5,
                                  0.4, 0.6, 0.5, 0.2,
                                  0.2, 0.2, 0.4, 0.4 ), nrow = 4, ncol = 4, byrow = TRUE)

arm_mask           <- matrix(  c( 1,   1,   1,   0,
                                  0,   1,   1,   1,
                                  1,   1,   0,   1,
                                  1,   0,   1,   1),    nrow = 4, ncol = 4, byrow = TRUE)

context_weights    <- matrix(  c( 0.4, 0.2, 0.2, 0.3,
                                  0.3, 0.5, 0.3, 0.2),  nrow = 2, ncol = 4, byrow = TRUE)

bandit             <- SyntheticBandit$new(arm_weights     = arm_weights,
                                          arm_mask        = arm_mask,
                                          context_weights = context_weights)

agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(LinUCBDisjointPolicy$new(), bandit),
                           Agent$new(LinUCBHybridPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

plot(history, type = "cumulative")

#h <- history$get_data_table()
#plot = Plot$new()
#plot_result = plot$arms( h[agent == "LinUCBHybrid"] )
#plot_result = plot$arms( h[agent == "UCB1"] )

