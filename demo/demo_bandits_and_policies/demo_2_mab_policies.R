library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


weight_per_arm     <- c(0.9, 0.1, 0.1)
horizon            <- 100
simulations        <- 500

bandit             <- SyntheticBandit$new(weights = weight_per_arm, precaching = TRUE)

agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(GittinsBrezziLaiPolicy$new(), bandit),

                           Agent$new(UCB1Policy$new(), bandit))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

par(mfrow = c(2,2),mar = c(5,5,1,1))

plot(history, type = "cumulative", regret = TRUE, no_par = TRUE, step_size = 4, use_colors = TRUE)
plot(history, type = "cumulative", regret = FALSE, no_par = TRUE, legend = FALSE,
                                step_size = 4, use_colors = TRUE)
plot(history, type = "average", ci = "ci", regret = FALSE, no_par = TRUE, legend = FALSE,
                                step_size = 10, use_colors = TRUE, smooth = TRUE)
plot(history, type = "arms", regret = FALSE, no_par = TRUE, legend = TRUE, step_size = 4, use_colors = TRUE)

par(mfrow = c(1,1))

print(history)
summary(history)

print(history$meta$sim_total_duration)
