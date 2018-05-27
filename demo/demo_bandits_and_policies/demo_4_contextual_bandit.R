library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")

# horizon            <- 1000L
# simulations        <- 10L
# bandit             <- ContextualBandit$new(k = 5, d = 6)
#
# agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
#                             Agent$new(ContextualThompsonSamplingPolicy$new(), bandit),
#                             Agent$new(LinUCBDisjointPolicy$new(0.7, "LinUCB"), bandit) )
#
# simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = FALSE)
#
# history            <- simulation$run()
#
# plot(history, type = "cumulative", rate = TRUE)

###

bandit      <- ContextualBandit$new(k = 5, d = 6, num_users = 7)

agents      <- list(Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                    Agent$new(ContextualThompsonSamplingPolicy$new(name = "ContextualTS"), bandit),
                    Agent$new(LinUCBDisjointPolicy$new(0.7, "LinUCBDisjoint"), bandit),
                    Agent$new(LinUCBHybridPolicy$new(0.7, 6, "LinUCBHybrid"), bandit))

simulation  <- Simulator$new(agents, 300, 100)
history     <- simulation$run()

# plot through contextual
plot(history, type = "cumulative", ci = TRUE, rate = TRUE)

# plot through ggplot2
library(ggplot2)
history_dt <- history$get_data_table() # extract data.table from the history object
max_sim <- history_dt[, max(sim)]
history_dt$cumsum <- history_dt[, cumsum(optimal_reward_value - reward)/t, by = list(agent, sim)]$V1
df <- history_dt[, list(sd = sd(cumsum), mean = mean(cumsum)), by = list(t, agent)]
ci_range <- df$mean + outer(df$sd/sqrt(max_sim), c(1.96, -1.96))
df <- cbind(df, ci_range)
print( ggplot(data = df, aes(x = t, y = mean, ymin = V1, ymax = V2)) +
         geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3) +
         theme(legend.justification = c(0,0), legend.position = c(0.845,0.57), legend.title = element_blank()) +
         labs(x = "Time step", y = "Cumulative regret - rate")) # plot the ggplot object



