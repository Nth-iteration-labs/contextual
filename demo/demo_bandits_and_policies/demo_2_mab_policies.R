library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


weight_per_arm     <- c(0.9, 0.1, 0.1)
horizon            <- 100L
simulations        <- 1000L

bandit             <- SyntheticBandit$new(weights = weight_per_arm, precaching = TRUE)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "EGreedy"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0, "TS"), bandit),
                            Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
                            Agent$new(UCB1Policy$new("UCB1"), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

par(mfrow = c(2,2),mar = c(5,5,1,1))
plot(history, type = "cumulative", regret = TRUE, no_par = TRUE)
plot(history, type = "cumulative", regret = FALSE, no_par = TRUE, legend = FALSE)
plot(history, type = "average", regret = FALSE, no_par = TRUE, legend = FALSE, ci = TRUE)
plot(history, type = "arms", regret = FALSE, no_par = TRUE, legend = TRUE)
par(mfrow = c(1,1))

print(history)
summary(history)
