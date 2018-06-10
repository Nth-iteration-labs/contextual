library(contextual)
library(here)
setwd(here("demo","demo_bandits_and_policies"))
source("../dev.R")


weight_per_arm     <- c(0.9, 0.1, 0.1)
horizon            <- 10L
simulations        <- 10L

bandit             <- SyntheticBandit$new(weights = weight_per_arm)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.2), bandit),
                            Agent$new(RandomPolicy$new(), bandit, "Random"),
                            Agent$new(OraclePolicy$new(), bandit, "Oracle"),
                            Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit, "TS"),
                            Agent$new(Exp3Policy$new(0.1), bandit, "Exp3"),
                            Agent$new(UCB1Policy$new(), bandit, "UCB1"))

simulation         <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history            <- simulation$run()

par(mfrow = c(2,2),mar = c(5,5,1,1))
plot(history, type = "cumulative", regret = TRUE, no_par = TRUE, ci = TRUE)
plot(history, type = "cumulative", regret = FALSE, no_par = TRUE, legend = FALSE)
plot(history, type = "average", regret = FALSE, no_par = TRUE, legend = FALSE, ci = TRUE)
plot(history, type = "arms", regret = FALSE, no_par = TRUE, legend = TRUE, limit_agents = c("TS"))
par(mfrow = c(1,1))

#print(history)
summary(history)

t <- history$get_data_table()
h <- history$get_cumulative_data()
c <- history$get_cumulative_final()
a <- history$get_meta_agent()
m <- history$get_meta_data()


print(m$sim_total_duration)
print(c$EpsilonGreedy$cum_regret_var)

print(history$meta$sim_total_duration)
print(history$cumulative$EpsilonGreedy$cum_regret_var)

