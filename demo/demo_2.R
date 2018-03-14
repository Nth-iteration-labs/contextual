setwd("~/GitHub/contextual/demo")
source("dev.R")
horizon            <- 3000
simulations        <- 50

# all is not well here yet :)

arm_weights        <- matrix(  c( 0.2, 0.7, 0.2, 0.7,
                                  0.5, 0.2, 0.7, 0.2), nrow = 2, ncol = 4, byrow = TRUE)


context_weights    <- matrix(  c( 0.7, 0.2, 0.2, 0.3,
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
