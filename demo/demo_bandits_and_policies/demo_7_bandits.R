library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

horizon       <- 5000L
simulations   <- 10L

delta         <- 0.95
num_actions   <- 5
context_dim   <- 2
mean_v        <- c(1.0, 1.0, 1.0, 1.0, 1.2)
std_v         <- c(0.05, 0.05, 0.05, 0.05, 0.05)
mu_large      <- 50
std_large     <- 0.01

bandit        <- ContextualWheelBandit$new(delta, mean_v, std_v, mu_large, std_large)
agents        <- list(Agent$new(UCB1Policy$new(), bandit),
                      Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                      Agent$new(LinUCBDisjointOptimizedPolicy$new(0.6), bandit))

simulation     <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history        <- simulation$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", smooth = T)
