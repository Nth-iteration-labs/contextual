setwd("~/GitHub/contextual/demo")
source("dev.R")

library("ggplot2")


weights         <- matrix(  c( 0.9, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.9),
                              nrow = 3, ncol = 3, byrow = TRUE)
bandit          <- SyntheticBandit$new(weights = weights)
agents          <- list(Agent$new(LinUCBDisjointPolicy$new(0.1, "LinUCB 0.5"), bandit),
                        Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit),
                        Agent$new(LinUCBDisjointPolicy$new(3.0, "LinUCB"), bandit))
history         <- Simulator$new(agents, 100, 100, do_parallel = TRUE)$run()

par(mfrow = c(1, 1), mar = c(2,4,1,1), oma = c(0,0,0,0))
plot(history, type = "average", regret = FALSE, ci = TRUE, no_par = TRUE)

history_dt <- history$get_data_table() # extract data.table from the history object
max_sim <- history_dt[, max(sim)]
df <- history_dt[, list(ci = sd(reward) / sqrt(max_sim), data = mean(reward)),
                 by = list(t, agent)]
ci_range <- df$data + outer(df$ci, c(1.96, -1.96))
df <- cbind(df, ci_range)
print( ggplot(data = df, aes(x = t, y = data, ymin = V1, ymax = V2)) +
         geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3) +
         labs(x = NULL, y = "Average reward"))
