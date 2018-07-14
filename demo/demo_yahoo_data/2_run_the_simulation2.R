library(here)
setwd(here("demo", "demo_yahoo_data"))
source("../dev.R")                          # TODO: check this, here not loaded, etc..
#library(contextual)

library(data.table)
library(DBI)
library(MonetDB.R)
library(here)
library(doParallel)

setwd(here("demo", "demo_yahoo_data"))

source("yahoo_bandit.R", encoding="utf-8")
source("yahoo_policy_epsilon_greedy.R", encoding="utf-8")
source("yahoo_policy_linucb_disjoint.R", encoding="utf-8")
source("yahoo_policy_linucb_hybrid.R", encoding="utf-8")
source("yahoo_policy_random.R", encoding="utf-8")

# During testing, if breaks during running..

doParallel::stopImplicitCluster()

# Connect to DB ----------------------------------------------------------------------------------------------

con <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname="yahoo", user="monetdb", password="monetdb")

message(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Config -----------------------------------------------------------------------------------------------------

# 50000 shoukd be enough to get an impression of CTR

simulations <- 1
horizon     <- 30000000


# Get arm/article lookup

arm_lookup <- as.matrix(DBI::dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
class(arm_lookup) <- "integer"
arm_lookup <- as.vector(arm_lookup)

#which(arm_lookup == 109596)
#match(c(109785,109476),arm_lookup)

# Initiate YahooBandit ---------------------------------------------------------------------------------------

# TODO: again, make conjoint and disjount 1:6 1:6 here..

bandit      <- YahooBandit$new(k = 217L, d = 6L, arm_lookup = arm_lookup, cache = 2000)  # TODO: make sure clear why if d 36 error

agents <-
  list(
    Agent$new(YahooLinUCBDisjointPolicy$new(0.2), bandit, name = "LinUCB Hyb"),
    Agent$new(YahooLinUCBHybridPolicy$new(0.2), bandit, name = "LinUCB Disj"),
    Agent$new(YahooEpsilonGreedyPolicy$new(0.2), bandit, name = "EGreedy"),
    Agent$new(YahooRandomPolicy$new(), bandit, name = "Random")
  )

# Define the simulation --------------------------------------------------------------------------------------

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    continuous_counter = TRUE,
    reindex_t = TRUE,            # TODO: if you reindex here, no chance later data back?
    write_progress_file = TRUE,
    include_packages = c("MonetDB.R")
  )

# Run the simulation

sim  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

print(sim$meta$sim_total_duration)  # TODO: I want average CTR, for example, and final CTR, and cleanup what you can see/do via $

# TODO: duration is now unclear in minutes, hours...

#print(length(sim$data$choice))

#summary(sim)

plot(sim, regret = FALSE, rate = TRUE, type = "cumulative")

#df <- sim$get_data_frame()

dbDisconnect(con)

