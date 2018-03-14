setwd("~/GitHub/contextual/demo")
source("dev.R")
horizon            <- 1000
simulations        <- 50

# all is not well here yet :)


# this would be the weights that randomize over all arms -> users
arm_weights        <- matrix(  c( 0.2, 0.7, 0.2, 0.7,
                                  0.5, 0.2, 0.7, 0.2), nrow = 2, ncol = 4, byrow = TRUE)

# this would be weights that randomize over arms -> articles
context_weights    <- matrix(  c( 0.7, 0.2, 0.2, 0.6,
                                  0.3, 0.5, 0.7, 0.2),  nrow = 2, ncol = 4, byrow = TRUE)

bandit             <- SyntheticBandit$new(arm_weights     = arm_weights,
                                          context_weights = context_weights)

agents             <- list(Agent$new(LinUCBDisjointPolicy$new(), bandit),
                           Agent$new(LinUCBHybridPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(), bandit),
                           Agent$new(ContextualThompsonSamplingPolicy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

plot(history, type = "cumulative")

#h <- history$get_data_table()
#plot = Plot$new()
#plot_result = plot$arms( h[agent == "LinUCBHybrid"] )
#plot_result = plot$arms( h[agent == "UCB1"] )
