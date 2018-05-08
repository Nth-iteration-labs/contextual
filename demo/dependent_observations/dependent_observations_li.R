##################### Imports ##########################

setwd("~/GitHub/contextual/demo/dependent_observations")

library(data.table)
library(contextual)
source("./bandit_dependent_li.R")
source("./policy_pooled_egreedy.R")

##################### Settings #########################

horizon        <- 570061
simulations    <- 1

data_dir       <- "data/"

data           <- fread(paste0(data_dir,"persuasion_detail_pages_only.csv"),
                     select = c("sid","uid","scs"),
                     nrows = (horizon*simulations))

setnames(data, old = c("sid","uid","scs"), new = c('choice','user','reward'))

data$user      <- as.numeric(factor(data$user, levels = unique(data$user)))
data$choice    <- data$choice + 1
users          <- data[, .N, keyby = user]
max_repeat     <- max(users$N)
n_users        <- length(users$user)

print(max_repeat)
print(n_users)

##################### Bandit ###########################

bandit      <- DependentObservationsLiBandit$new(data, 4)

##################### Egreedy Policies #################

agents      <- list( Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users, name = "UnpooledEG"), bandit),
                     Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1, name = "PooledEG"), bandit),
                     Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = n_users, name = "PartialEG"), bandit))

history     <- Simulator$new(agents = agents,
                             horizon = horizon,
                             simulations = simulations,
                             reindex_t = TRUE,
                             do_parallel = TRUE)$run()

##################### Plot #############################

plot(history, type = "cumulative", regret = FALSE, rate = TRUE )







