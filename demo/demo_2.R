setwd("~/GitHub/contextual/demo")
source("dev.R")

# naming!!! we have feature matrix X ... with arm features, and context features.
# so matrix X, with d nr total features. also, with d_a arm features, and d_c context feautres
# also, sub-feature matrices X_a and X_c .. and vectors x_a and x_c etc.
# or something along these lines.

# Here, we simulate a 4 armed bandit, k=4.
# Per arm, there are 4 arm-based features,
# for example, if arms are articles:
# article== sports 1|0 / article == science 1|0, etc
# Also, there is a set of 2 features that are specific
# for a particular time step, for example,
# a user visiting is male, language browser is english, etc
# This adds up to total of *6* context features d=6 every round

horizon             <- 500
simulations         <- 500

arm_weights         <- matrix(  c( 0.3, 0.8, 0.4, 0.8,
                                   0.3, 0.2, 0.2, 0.5,
                                   0.4, 0.6, 0.5, 0.2,
                                   0.2, 0.2, 0.4, 0.4 ), nrow = 4, ncol = 4, byrow = TRUE)

arm_masks           <- matrix(  c( 1,   1,   1,   0,
                                   0,   1,   1,   1,
                                   1,   1,   0,   1,
                                   1,   0,   1,   1),   nrow = 4, ncol = 4, byrow = TRUE)

context_weights     <- matrix(  c( 0.4, 0.2, 0.2, 0.3,
                                   0.3, 0.5, 0.3, 0.2), nrow = 2, ncol = 4, byrow = TRUE)

bandit             <- SyntheticBandit$new(arm_weights     = arm_weights,
                                          arm_masks       = arm_masks,
                                          context_weights = context_weights)

agents             <- list(Agent$new(LinUCBDisjointPolicy$new(), bandit),
                           Agent$new(LinUCBHybridPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

plot(history, type = "cumulative")

h <- history$get_data_table()
plot = Plot$new()
plot_result = plot$arms( h[agent == "LinUCBHybrid"] )
plot_result = plot$arms( h[agent == "UCB1"] )

