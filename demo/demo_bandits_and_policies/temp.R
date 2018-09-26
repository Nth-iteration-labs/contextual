library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")




bandit             <- ContextualBernoulliBandit$new(weights = c(0.9, 0.1, 0.1))

agents             <- list(Agent$new(RandomPolicy$new(), bandit),
                           Agent$new(OraclePolicy$new(), bandit),
                           Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                           Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(GittinsBrezziLaiPolicy$new(), bandit),
                           Agent$new(UCB1Policy$new(), bandit))

history            <- Simulator$new(agents, horizon = 10, simulations = 10,
                                    do_parallel = FALSE, save_context = TRUE)$run()

plot(history, traces_alpha = 0.2, traces_max = 2, traces = TRUE)

plot(history, traces_alpha = 0.2, traces_max = 2, lwd = 1, traces = TRUE)

plot(history, color_step  = 2, lty_step = 2)

plot(history, legend_labels = c(1:5), legend_title = "Policies")

plot(history, ylim = c(0.3,3.5), xlim = c(1,20))

plot(history, type = "arms", ylim = c(10,80), xlim = c(2,9))


record_plot <-  function() {
  plot(1:10)
  invisible(recordPlot())
}
p <- record_plot()
