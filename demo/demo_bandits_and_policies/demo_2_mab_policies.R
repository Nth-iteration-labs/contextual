library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


prob_per_arm     <- c(0.9, 0.1, 0.1)
horizon            <- 100
simulations        <- 100

bandit             <- BasicBernoulliBandit$new(prob_per_arm)

bandit             <- BasicGaussianBandit$new(c(0,0,1),c(1,1,1))


agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           #Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                           Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit)
)

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

par(mfrow = c(3, 2), mar = c(3, 5, 1, 1))

plot(history,
     type = "cumulative",
     no_par = TRUE,
     use_colors = FALSE)

plot(
  history,
  type = "cumulative",
  regret = FALSE,
  legend = FALSE,
  limit_agents = c("UCB1"),
  traces = TRUE,
  no_par = TRUE
)

plot(
  history,
  type = "cumulative",
  regret = FALSE,
  rate = TRUE,
  ci = "sd",
  limit_agents = c("Exp3", "ThompsonSampling"),
  legend_position = "bottomright",
  no_par = TRUE
)

plot(
  history,
  type = "cumulative",
  rate = TRUE,
  plot_only_ci = TRUE,
  ci = "var",
  limit_agents = c("UCB1", "GittinsBrezziLai"),
  legend_position = "topright",
  smooth = TRUE,
  no_par = TRUE
)

plot(
  history,
  type = "average",
  ci = "ci",
  regret = FALSE,
  interval = 10,
  use_colors = TRUE,
  legend_position = "bottomright",
  smooth = TRUE,
  no_par = TRUE
)

plot(
  history,
  limit_agents = c("ThompsonSampling"),
  type = "arms",
  interval = 20,
  no_par = TRUE
)

par(mfrow = c(1, 1))

print(history)
summary(history)

