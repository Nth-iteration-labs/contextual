library(contextual)
library(here)
library(data.table)
setwd(here("demo", "demo_criteo_linucb"))
source("../dev.R")

################ Phase I - Importing and parsing Criteo data #############

# import critea data
critea_data_table   <- fread("dataset.txt", sep = " ")

# retrieve number of rows  (which equals the horizon)
critea_horizon      <- nrow(critea_data_table)

# move all context columns to context vectors, while deleting the original columns
critea_data_table[, context := as.list(as.data.frame(t(critea_data_table[, 3:102])))][, (3:102) := NULL]

# add t, sim and agent columns
critea_data_table[, t := .I]
critea_data_table[, sim := 1]
critea_data_table[, agent := "Criteo"]

# name choice and reward columns
setnames(critea_data_table, c("V1", "V2"), c("choice", "reward"))

# create history object from data.table
critea_history <- History$new()
critea_history$set_data_table(critea_data_table, auto_stats = FALSE)
rm(critea_data_table)

################ Phase II - Running Li Bandit ############################

simulations <- 1
horizon     <- critea_horizon

# initiate Li bandit, with 10 arms, and 100 dimensions
bandit      <-
  LiSamplingOfflineBandit$new(data_stream = critea_history, k = 10, d = 100)

# define two LinUCBDisjointSmPolicy agents
agents <-
  list(
    Agent$new(LinUCBDisjointSmPolicy$new(0.001), bandit, name = "LinUCB a=0.001"),
    Agent$new(LinUCBDisjointSmPolicy$new(1.0), bandit, name = "LinUCB a=1.0")
  )

# define the simulation
simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = critea_horizon,
    do_parallel = TRUE,
    continuous_counter = TRUE,
    reindex_t = TRUE
  )

# run the simulation
criteo_sim  <- simulation$run()

################ Phase III - Take a look at the results ##################

# total duration sim
print(criteo_sim$meta$sim_total_duration)

# plot the results
plot(criteo_sim,
     regret = FALSE,
     rate = TRUE,
     type = "cumulative")

