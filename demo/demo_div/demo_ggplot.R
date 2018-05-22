setwd("~/GitHub/contextual/demo")
source("dev.R")

# lets test ggplot

library(ggplot2)

horizon            <- 70L
simulations        <- 5000L
context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3, byrow = TRUE)

bandit             <- SyntheticBandit$new(weights = context_weights)

agents             <- list( Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

# first, plot with contextual plot

ptm <- proc.time()
plot(history, type = "average", regret = FALSE, ci = TRUE)
print(proc.time() - ptm)

# now, the same plot with ggplot

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

