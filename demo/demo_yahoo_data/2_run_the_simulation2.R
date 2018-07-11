source("../dev.R")
#library(contextual)
library(data.table)
library(DBI)
library(MonetDB.R)
library(here)

setwd(here("demo", "demo_yahoo_data"))

source("yahoo_bandit.R")
source("yahoo_policy_epsilon_greedy.R")
source("yahoo_policy_linucb_disjoint.R")

# Connect to DB ----------------------------------------------------------------------------------------------

options(monetdb.sequential=F)

con <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname="yahoo", user="monetdb", password="monetdb")

message(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Config -----------------------------------------------------------------------------------------------------

simulations <- 1
horizon     <- 10000000

# Get arm/article lookup

arms_articles <- as.matrix(DBI::dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
class(arms_articles) <- "integer"

# Initiate YahooBandit ---------------------------------------------------------------------------------------

bandit      <- YahooBandit$new(k = 217L, d = 12L, arm_lookup = arms_articles, cache = 1000)  # TODO: make sure clear why if d 36 error

agents <-
  list(
    Agent$new(YahooLinUCBDisjointPolicy$new(0.1), bandit, name = "LinUCB"),
    Agent$new(YahooEpsilonGreedyPolicy$new(0.1), bandit, name = "EGreedy")
  )

# Define the simulation --------------------------------------------------------------------------------------

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    continuous_counter = TRUE,
    reindex_t = TRUE,                    # TODO: if you reindex here, no chance later data back?
    write_progress_file = FALSE,
    include_packages = c("MonetDB.R")
  )

# Run the simulation

sim  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

print(sim$meta$sim_total_duration)

plot(sim, regret = FALSE, rate = FALSE, type = "cumulative")

df <- sim$get_data_frame()

dbDisconnect(con)

