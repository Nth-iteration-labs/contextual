##################### Imports ##########################

setwd("~/GitHub/contextual/demo/dependent_observations")

library(data.table)
library(contextual)
source("./bandit_dependent_li.R")
source("./policy_pooled_egreedy.R")

data_dir       <- "data/"

##################### Settings #########################
#570000
horizon        <- 570000
simulations    <- 100

data           <- fread(paste0(data_dir,"persuasion_detail_pages_only.csv"),
                     select = c("sid","uid","scs"),
                     #nrows = (horizon*simulations))
                     nrows = horizon)

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
                             #continouous_counter = TRUE,
                             reindex_t = TRUE,
                             do_parallel = TRUE)$run()

history$save_data(paste0(data_dir,"LiEG_1000.RData"))

##################### Plot #############################

setwd("~/GitHub/contextual/demo/dependent_observations")

source("dev.R")

data_dir       <- "data/"

history = History$new()
history$clear_data_table()
history$load_data(paste0(data_dir,"LiEG_all_1000.RData"))

h <- history$get_data_table()

h$agent <- factor(h$agent, levels = list("PartialEG","UnpooledEG", "PooledEG"))
plot.History(h, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.01,0.016))


