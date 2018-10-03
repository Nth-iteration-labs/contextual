library(contextual)
source("../dev.R")

library(data.table)
library(DBI)
library(MonetDBLite)
library(here)


setwd(here::here("demo", "replication_li_2010"))

source("yahoo_bandit.R")
source("yahoo_policy_epsilon_greedy.R")

# Connect to DB ----------------------------------------------------------------------------------------------

# monetdb.sequential=T is the difference between monetdblite life and death

options(monetdb.sequential=T)

db_dir <- "C:/YahooDb/yahoo.monetdblite"

con    <- dbConnect(MonetDBLite::MonetDBLite(), db_dir)

print(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Config -----------------------------------------------------------------------------------------------------

simulations <- 1
horizon     <- 10000

counted_rows <- as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" ))
max_t        <- as.integer(DBI::dbGetQuery(con, "SELECT max(t) FROM yahoo" ))

print(counted_rows == max_t)

# Get arm/article lookup

arms_articles <- as.matrix(DBI::dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
class(arms_articles) <- "integer"

# Initiate YahooBandit ---------------------------------------------------------------------------------------

bandit      <- YahooBandit$new(con, k = 217L, d = 36L, arm_lookup = arms_articles)

agents <-
  list(
    Agent$new(YahooEpsilonGreedyPolicy$new(0.01), bandit, name = "EGreedy")
  )

# Define the simulation --------------------------------------------------------------------------------------

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = FALSE,
    reindex = TRUE,
    write_progress_file = TRUE,
    include_packages = c("DBI","MonetDBLite")
  )

# Run the simulation

sim  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

print(sim$meta$sim_total_duration)

plot(sim, regret = FALSE, rate = FALSE, type = "cumulative")

df <- sim$get_data_frame()

dbDisconnect(con, shutdown = TRUE)

