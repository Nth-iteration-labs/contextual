library(contextual)
library(data.table)

library(DBI)
library(MonetDBLite)
library(here)

source("../dev.R")

setwd(here("demo", "demo_yahoo_data"))

source("yahoo_bandit.R")
source("yahoo_policy_epsilon_greedy.R")

# Connect to DB ----------------------------------------------------------------------------------------------

db_dir <- "C:/YahooDb/yahoo.monetdblite"

# First time connection can take some time.

con    <- dbConnect(MonetDBLite::MonetDBLite(), db_dir)

print(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Config -----------------------------------------------------------------------------------------------------

simulations <- 1
horizon     <- 1000

counted_rows <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" ))
max_t        <- as.integer(dbGetQuery(con, "SELECT max(t) FROM yahoo" ))

print(counted_rows == max_t)

# Get arm/article lookup

if(!file.exists("./cache/arm_article.Rds")){
  arm_article <- as.data.table(dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
  arm_article <- as.matrix(arm_article)
  class(arm_article) <- "integer"
  saveRDS(arm_article, file = "./cache/arm_article.Rds")
} else {
  if(!exists("arm_article")) arm_article <- readRDS(file = "./cache/arm_article.Rds")
}

# Initiate YahooBandit ---------------------------------------------------------------------------------------

bandit      <- YahooBandit$new(con, k = 217L, d = 36L)

agents <-
  list(
    Agent$new(YahooEpsilonGreedyPolicy$new(0.01), bandit, name = "EGreedy"),
    Agent$new(YahooEpsilonGreedyPolicy$new(0.1), bandit, name = "EGreedy2")
  )

# Define the simulation --------------------------------------------------------------------------------------

simulation <-
  Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    continuous_counter = TRUE,
    reindex_t = TRUE,
    include_packages = c("DBI","MonetDBLite")
  )

# Run the simulation

sim  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

print(sim$meta$sim_total_duration)

plot(sim, regret = FALSE, rate = FALSE, type = "cumulative")

df <- sim$get_data_frame()

dbDisconnect(con, shutdown = TRUE)

