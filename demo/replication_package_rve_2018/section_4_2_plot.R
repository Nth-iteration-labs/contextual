library(contextual)

bandit <- ContextualBernoulliBandit$new(weights = c(0.9, 0.1, 0.1))
agents <- list(Agent$new(RandomPolicy$new(), bandit),
               Agent$new(OraclePolicy$new(), bandit),
               Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
               Agent$new(Exp3Policy$new(0.1), bandit),
               Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
               Agent$new(UCB1Policy$new(), bandit))
history <- Simulator$new(agents, horizon = 100, simulations = 1000)$run()

par(mfrow = c(3, 2), mar = c(1, 4, 2, 1), cex=1.3)  #bottom, left, top, and right.
plot(history, type = "cumulative", use_colors = FALSE, no_par = TRUE, legend_border = FALSE,
     limit_agents = c("GittinsBrezziLai", "UCB1","ThompsonSampling"))
plot(history, type = "cumulative", regret = FALSE, legend = FALSE,
     limit_agents = c("UCB1"), traces = TRUE, no_par = TRUE)
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, disp = "sd",
     limit_agents = c("Exp3", "ThompsonSampling"),
     legend_position = "bottomright", no_par = TRUE)
plot(history, type = "cumulative", rate = TRUE, plot_only_disp = TRUE,
     disp = "var", smooth = TRUE, limit_agents = c("UCB1", "GittinsBrezziLai"),
     legend_position = "bottomleft", no_par = TRUE)
plot(history, type = "average", disp = "ci", regret = FALSE, interval = 10,
     smooth = TRUE, legend_position = "bottomright", no_par = TRUE, legend = FALSE)
plot(history, limit_agents = c("ThompsonSampling"), type = "arms",
     interval = 20, no_par = TRUE)
par(mfrow = c(1, 1))
