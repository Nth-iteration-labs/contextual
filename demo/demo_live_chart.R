setwd("~/GitHub/contextual/demo")
#library(contextual)
source("dev.R")

set.seed(21L)

plot <- Plot$new()                                                              # Initialize live plot. TODO: change to within class.
plot$set_external(T, 11L, 6L)                                                   # Set plot to external window when in Rstudio.

agents <- list()                                                                # A list to keep track of our agents

bandit  = SyntheticBandit$new(
  k = 3L,
  d = 1L,
  weight_distribution = "Uniform",
  feature_type =        "Bernoulli"
)
                     #d1
bandit$set_weights(c(0.2,  #k1
                     0.4,  #k2                                                  # d is the number of feature,
                     0.8)) #k3                                                  # k is the number of arms.

policyEG       <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")
agents$EG      <- Agent$new(policyEG, bandit)

policyTS       <- ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")
agents$TS      <- Agent$new(policyTS, bandit)

policyRandom   <- RandomPolicy$new("Random")
agents$Random  <- Agent$new(policyRandom, bandit)


simulation     <- SimulatorBasic$new(agents,
                                     animate = TRUE,
                                     animate_step = 1L,
                                     horizon = 100L,
                                     simulations = 100L)

history        <- simulation$run()
