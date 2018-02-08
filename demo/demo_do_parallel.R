setwd("~/GitHub/contextual/demo")
source("dev.R")

library(ggplot2)

bandit <- SyntheticBandit$new(reward_family = "Bernoulli", seed = 1, precache = TRUE)

                             #k1   #k2   #k3
bandit$set_weights(matrix(c( 0.9,  0.1,  0.1,
                             0.1,  0.2,  0.1,
                             0.2,  0.1,  0.2 ), nrow = 3L, ncol = 3L ))

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

simulation     <- Simulator$new(
  agents,
  horizon      = 200L,
  simulations  = 1000L,
  save_context = FALSE,
  save_theta   = FALSE,
  do_parallel  = TRUE
)

history <- simulation$run()

ptm <- proc.time()
plot(history, type = "average", regret = FALSE, ci = TRUE)
print(proc.time() - ptm)

ptm <- proc.time()
history_dt <- history$get_data_table()
max_sim   = history_dt[, max(sim)]
cs <- history_dt[, list(sd = sd(reward) / sqrt(max_sim), data = mean(reward)), by = list(t, agent)]
ci_range <- cs$data + outer(cs$sd, c(1.96, -1.96))
cs = cbind(cs, ci_range)
colnames(cs)[colnames(cs) == 'V2'] <- 'ci_lower'
colnames(cs)[colnames(cs) == 'V1'] <- 'ci_upper'
ptm <- proc.time()
gp <- ggplot(data = cs, aes(
    x = t,
    y = data,
    ymin = ci_lower,
    ymax = ci_upper
  )) +
  geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3)
print(gp)
print(proc.time() - ptm)

