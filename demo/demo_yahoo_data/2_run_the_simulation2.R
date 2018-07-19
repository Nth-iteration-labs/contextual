library(here)
setwd(here("demo", "demo_yahoo_data"))
source("../dev.R")                          # TODO: check this, here not loaded, etc..
#library(contextual)

library(data.table)
library(DBI)
library(MonetDB.R)
library(here)
library(doParallel)

library(RevoUtilsMath)

setwd(here("demo", "demo_yahoo_data"))

source("yahoo_bandit.R", encoding="utf-8")
source("yahoo_policy_epsilon_greedy.R", encoding="utf-8")
source("yahoo_policy_ucb1_alpha.R", encoding="utf-8")
source("yahoo_policy_linucb_disjoint.R", encoding="utf-8")
source("yahoo_policy_linucb_hybrid.R", encoding="utf-8")
source("yahoo_policy_random.R", encoding="utf-8")

doParallel::stopImplicitCluster()

try(RevoUtilsMath::setMklthreads(1), silent=TRUE)  # TODO: in package itself?

# Connect to DB ----------------------------------------------------------------------------------------------

con <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname="yahoo", user="monetdb", password="monetdb")
message(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Config -----------------------------------------------------------------------------------------------------

simulations <- 1
horizon     <- 10000000

# Eq 4 from Li2010 - may be conservatively large in some applications
# therefor, we choose the values based on the Li2010 plot, HybridLinUCB 0.4, DisjointLinUCB 0.2
# delta = 0.05 #Eq 4 from Li2010
# alpha = 1 + sqrt(log(2/delta)/2)

# Get arm/article lookup

arm_lookup <- as.matrix(DBI::dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
class(arm_lookup) <- "integer"
arm_lookup <- rev(as.vector(arm_lookup))

# Initiate YahooBandit ---------------------------------------------------------------------------------------

# TODO: again, make conjoint and disjount 1:6 and 7:12 here in a smart way

bandit      <- YahooBandit$new(k = 217L, d = 6L, arm_lookup = arm_lookup, cache = 1000)

agents <-
  list(
        Agent$new(YahooLinUCBDisjointPolicy$new(0.2,0.99), bandit, name = "LinUCB Dis"),
        Agent$new(YahooLinUCBHybridPolicy$new(0.4,0.99),   bandit, name = "LinUCB Hyb"),
        Agent$new(YahooEpsilonGreedyPolicy$new(0.3,0.99),  bandit, name = "EGreedy"),
        Agent$new(YahooUCB1AlphaPolicy$new(0.4,0.99),      bandit, name = "UCB1"),
        Agent$new(YahooUCB1AlphaPolicy$new(0.01,0.99,T),   bandit, name = "UCB1Auer"),
        Agent$new(YahooRandomPolicy$new(),              bandit, name = "Random")
      )

# Define the simulation --------------------------------------------------------------------------------------

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    continuous_counter = TRUE,
    reindex_t = TRUE,                # TODO: think what about a really bad bandit, never good .. then... nothing to see - then you want to reindex, but do not want to delete overspan - overspan is in general something that should be left to the plot function, not here..
    write_progress_file = TRUE,
    include_packages = c("MonetDB.R")
  )

# Run the simulation

sim  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

print(sim$meta$sim_total_duration)  # TODO: I want average CTR, for example, and final CTR, and cleanup what you can see/do via $

#print(length(sim$data$choice))

#summary(sim)

plot(sim, regret = FALSE, rate = TRUE, type = "cumulative")

# TODO: print ctr (or relative cum) .. as on of the default outcomes in summary.. and replace print by summary

# sim$save_data("all_data.Rds")

dbDisconnect(con)






