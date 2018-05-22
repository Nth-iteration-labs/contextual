setwd("~/GitHub/contextual/demo")
source("dev.R")

# run a simulation
horizon            <- 25
simulations        <- 300
weights            <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.8, 0.1,
                                  0.1, 0.1, 0.7), nrow = 3, ncol = 3, byrow = TRUE)
bandit             <- SyntheticBandit$new(weights = weights)
agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1,"EG"), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )
simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
history            <- simulation$run()

# now, first, plot with contextual's default plot
plot(history, type = "average", regret = FALSE, ci = TRUE)

# next, the same plot using ggplot
library(ggplot2)
history_dt <- history$get_data_table()
max_sim   = history_dt[, max(sim)]
cs <- history_dt[, list(sd = sd(reward) / sqrt(max_sim), data = mean(reward)), by = list(t, agent)]
ci_range <- cs$data + outer(cs$sd, c(1.96, -1.96))
cs = cbind(cs, ci_range)
gp <- ggplot(data = cs, aes(x = t, y = data, ymin = V1, ymax = V2)) +
  geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3) +
  labs(x = "Time step", y = "Average reward")
print(gp)

