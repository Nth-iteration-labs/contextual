library(contextual)
library(here)
library(data.table)

setwd(here::here("demo","replication_kruijswijk_2018"))

source("./policy_pooled_egreedy.R")
source("./policy_pooled_ucb.R")
source("./bandit_clustered_bootstrapped_replay.R")


simulations <- 1
horizon     <- 5700

csv_url     <- "http://d1ie9wlkzugsxr.cloudfront.net/data_persuasion_api/persuasion_api_simple.csv"
data        <- fread(csv_url, nrows = horizon)

tab <- table(data$user)
data <- data[data$user %in% names(tab)[tab>5 & tab<50],]
#data <- data[data$user %in% names(tab)[tab<50],]
data$user <- as.numeric(as.factor(data$user))

#data$user   <- as.numeric(factor(data$user, levels = unique(data$user)))
#data$choice <- data$choice

users       <- data[, .N, keyby = user]

max_repeat  <- max(users$N)
n_users     <- length(users$user) * 10 # 10x is hacky -> but works

##################### Bandit ###########################

bandit      <- DependentObservationsClusteredBootstrappedBandit$new(data, 4)

##################### Policies #########################

# agents      <- list(Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit),
#                     Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1), bandit),
#                     Agent$new(RandomPolicy$new(), bandit),
#                     Agent$new(PartiallyBBPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit),
#                     Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users), bandit))

agents      <- list(#Agent$new(UnpooledUCBPolicy$new( n_subjects = n_users), bandit),
  #Agent$new(PooledUCBPolicy$new(), bandit),
  #Agent$new(RandomPolicy$new(), bandit),
  #Agent$new(PartiallyBBPooledUCBPolicy$new( n_subjects = n_users), bandit),
  Agent$new(PartiallyPooledUCBPolicy$new(n_subjects = n_users), bandit))

history     <- Simulator$new(agents = agents, horizon = horizon, do_parallel = FALSE,
                             simulations = simulations, save_interval = 50, include_packages = c("plyr"))$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.005,0.03))
