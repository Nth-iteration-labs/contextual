library(contextual)
library(here)
library(data.table)

setwd(here::here("demo","replication_kruijswijk_2018"))

source("./policy_pooled_egreedy.R")
source("./policy_pooled_ucb.R")
source("./bandit_bootstrapped_replay.R")


simulations <- 100
horizon     <- 570000

csv_url     <- "http://d1ie9wlkzugsxr.cloudfront.net/data_persuasion_api/persuasion_api_simple.csv"
data        <- fread(csv_url, nrows = horizon)

tab <- table(data$user)
data <- data[data$user %in% names(tab)[tab>5 & tab<50],]
data$user <- as.numeric(as.factor(data$user))

users       <- data[, .N, keyby = user]

max_repeat  <- max(users$N)
n_users     <- length(users$user)

print(max_repeat)
print(n_users)

# table(data$choice)
# 1      2      3      4
# 142628 142860 142439 142073

##################### Bandit ###########################

bandit      <- DependentObservationsBootstrappedBandit$new(data, 4)

##################### Policies #########################

agents      <- list(Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit),
                    Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1), bandit),
                    Agent$new(RandomPolicy$new(), bandit),
                    Agent$new(PartiallyBBPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit),
                    Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit))

history     <- Simulator$new(agents = agents, horizon = horizon, do_parallel = TRUE,
                             simulations = simulations, save_interval = 50, include_packages = c("plyr"))$run()

history$save("emp_eg_100reps.RData")
history$clear_data_table()
rm(agents,history)

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.01,0.0375), legend_border = FALSE, legend_position = "topright", ylab = "Click-through rate", legend_labels = c("No pooling", "Complete pooling", "Random", "Partial BB Pooling", "Partial Pooling"))

agents      <- list(Agent$new(UnpooledUCBPolicy$new( n_subjects = n_users), bandit),
                    Agent$new(PooledUCBPolicy$new(), bandit),
                    Agent$new(RandomPolicy$new(), bandit),
                    Agent$new(PartiallyBBPooledUCBPolicy$new( n_subjects = n_users), bandit),
                    Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = n_users), bandit)
)

history     <- Simulator$new(agents = agents, horizon = horizon, do_parallel = TRUE,
                             simulations = simulations, save_interval = 50, include_packages = c("plyr"))$run()
history$save("emp_ucb_100reps.RData")

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.01,0.0375), legend_border = FALSE, legend_position = "topright", ylab = "Click-through rate", legend_labels = c("No pooling", "Complete pooling", "Random", "Partial BB Pooling", "Partial Pooling"))
