source("../dev.R")
#library(contextual)
library(here)
library(data.table)
setwd(here("demo", "demo_criteo_linucb"))

################ Phase I - Importing and parsing Criteo data #############

# import critea data

critea_dt   <- fread("dataset.txt", sep = " ")

# retrieve number of rows  (which equals the horizon)

critea_horizon      <- 1000#nrow(critea_dt)

# move all context columns to context vectors

critea_dt[, context := as.list(as.data.frame(t(critea_dt[, 3:102])))]
critea_dt[, (3:102) := NULL]

# add t, sim and agent columns

critea_dt[, t := .I]
critea_dt[, sim := 1]
critea_dt[, agent := "Criteo"]

# name choice and reward columns

setnames(critea_dt, c("V1", "V2"), c("choice", "reward"))

# create history object from data.table

history <- History$new()
history$set_data_table(critea_dt, auto_stats = FALSE)
rm(critea_dt)


################ Phase II - Running Li Bandit ############################

simulations <- 1
horizon     <- critea_horizon

# initiate Li bandit, with 10 arms, and 100 dimensions

bandit      <- LiSamplingOfflineBandit$new(data_stream = history, k = 10, d = 100)

# define two LinUCBDisjointSmPolicy agents

agents <-
  list(
    Agent$new(LinUCBDisjointSmPolicy$new(0.001), bandit, name = "LinUCB alpha = 0.001"),
    Agent$new(LinUCBDisjointSmPolicy$new(0.01), bandit, name = "LinUCB alpha = 0.01"),
    Agent$new(LinUCBDisjointSmPolicy$new(0.1), bandit, name = "LinUCB alpha = 0.1"),
    Agent$new(LinUCBDisjointSmPolicy$new(1.0), bandit, name = "LinUCB alpha = 1.0")
  )

# define the simulation

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = critea_horizon,
    do_parallel = FALSE,
    worker_max = 2,
    continuous_counter = TRUE,
    reindex_t = TRUE,
    write_progress_file = FALSE
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

