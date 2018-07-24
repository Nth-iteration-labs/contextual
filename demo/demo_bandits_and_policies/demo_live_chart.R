library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


plot <- Plot$new()
plot$set_external(T, 11L, 6L)

agents <- list()

bandit  = SyntheticBandit$new(
  reward_family =         "Bernoulli"
)
                     #k1  #k2  #k3
bandit$set_weights(c(0.2, 0.4, 0.8))

policyEG       <- EpsilonGreedyPolicy$new(0.1, "\U190-greedy")
agents$EG      <- Agent$new(policyEG, bandit)

policyTS       <- ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")
agents$TS      <- Agent$new(policyTS, bandit)

policyRandom   <- RandomPolicy$new("Random")
agents$Random  <- Agent$new(policyRandom, bandit)


simulation     <- Simulator$new(agents,
                                     #animate = FALSE,
                                     #animate_step = 1L,
                                     horizon = 100L,
                                     simulations = 100L)

history        <- simulation$run()
