library(here)
library(data.table)
setwd(here("demo", "demo_offline_linucb"))
source("../dev.R")
#library(contextual)
library(here)
library(data.table)
setwd(here("demo", "demo_offline_linucb"))

################ Phase I - Importing and parsing linucb data #############

# import linucb data

url        <- "https://raw.githubusercontent.com/Nth-iteration-labs/contextual_data/master/data_cmab_basic/"

file       <- "dataset.txt"

linucb_dt  <- fread(paste0(url,file))

# retrieve number of rows  (which equals the horizon)

horizon    <- nrow(linucb_dt)

# move all context columns to context vectors

linucb_dt[, context := as.list(as.data.frame(t(linucb_dt[, 3:102])))]
linucb_dt[, (3:102) := NULL]

# add t, sim and agent columns

linucb_dt[, t := .I]
linucb_dt[, sim := 1]
linucb_dt[, agent := "linucb"]

# name choice and reward columns

setnames(linucb_dt, c("V1", "V2"), c("choice", "reward"))

################ Phase II - Running Li Bandit ############################

simulations <- 1
horizon     <- horizon

# initiate Li bandit, with 10 arms, and 100 dimensions

log_S       <- linucb_dt

bandit      <- OfflinePolicyEvaluatorBandit$new(data_stream = log_S, k = 10, d = 100)

# define two LinUCBDisjointSmPolicy agents

agents <-
  list(

    Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01), bandit, name = "LinUCB alpha = 0.01"),
    Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit, name = "LinUCB alpha = 0.05"),
    Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1), bandit, name = "LinUCB alpha = 0.1"),
    Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0), bandit, name = "LinUCB alpha = 1.0")
  )

# define the simulation

simulation <-
  Simulator$new(
    agents = agents,
    simulations = simulations,
    horizon = horizon,
    worker_max = 3,
    save_context = TRUE,
    reindex = TRUE
  )

# run the simulation
linucb_sim  <- simulation$run()

################ Phase III - Take a look at the results ##################

# plot the results

plot(linucb_sim,
     regret = FALSE,
     rate = TRUE,
     legend_position = "bottomright",
     type = "cumulative")

linucb_sim$save_csv(NA,context_to_columns = TRUE)

dt <- linucb_sim$get_data_table()

df <- linucb_sim$get_data_frame(context_to_columns = TRUE)
