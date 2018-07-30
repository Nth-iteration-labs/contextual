library(here)
library(contextual)
library(data.table)
library(DBI)
library(MonetDB.R)
library(doParallel)
library(RPushbullet)

# Config -----------------------------------------------------------------------------------------------------

simulations             <- 1
horizon                 <- 37.45e6
buffer_size             <- 1000
sparsity                <- 0.99
worker_max              <- 6

save_file_name          <- paste0("Yahoo_T_",horizon,"_sparse_",sparsity,".RData")

monetdb_host            <- "monetdb_ip"
monetdb_dbname          <- "yahoo"
monetdb_user            <- "monetdb"
monetdb_password        <- "monetdb"

setwd(here("demo", "replication_li_2010"))

# Setup ------------------------------------------------------------------------------------------------------

doParallel::stopImplicitCluster()

source("./demo_yahoo_classes/yahoo_bandit.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_epsilon_greedy.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_epsilon_greedy_seg.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_ucb1_alpha.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_ucb1_alpha_seg.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_linucb_unique.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_linucb_hybrid.R", encoding="utf-8")
source("./demo_yahoo_classes/yahoo_policy_random.R", encoding="utf-8")

# Connect to DB ----------------------------------------------------------------------------------------------

con <- DBI::dbConnect(MonetDB.R(), host=monetdb_host, dbname=monetdb_dbname,
                                   user=monetdb_user, password=monetdb_password)

message(paste0("MonetDB: connection to '",dbListTables(con),"' database succesful!"))

arm_lookup_table <- as.matrix(DBI::dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
class(arm_lookup_table) <- "integer"
arm_lookup_table <- rev(as.vector(arm_lookup_table))

# Initiate YahooBandit ---------------------------------------------------------------------------------------

bandit <- YahooBandit$new(k = 217L, unique = c(1:6), shared = c(7:12),
                          arm_lookup = arm_lookup_table, host = monetdb_host,
                          dbname = monetdb_dbname, user = monetdb_user,
                          password = monetdb_password, buffer_size = buffer_size)

agents <-
  list (Agent$new(YahooLinUCBDisjointPolicy$new(0.2),       bandit, name = "LinUCB Dis",  sparse = sparsity),
        Agent$new(YahooLinUCBHybridPolicy$new(0.2),         bandit, name = "LinUCB Hyb",  sparse = sparsity),
        Agent$new(YahooEpsilonGreedyPolicy$new(0.3),        bandit, name = "EGreedy",     sparse = sparsity),
        Agent$new(YahooEpsilonGreedySegPolicy$new(0.3),     bandit, name = "EGreedySeg",  sparse = sparsity),
        Agent$new(YahooUCB1AlphaPolicy$new(0.4),            bandit, name = "UCB1",        sparse = sparsity),
        Agent$new(YahooUCB1AlphaSegPolicy$new(0.4),         bandit, name = "UCB1Seg",     sparse = sparsity),
        Agent$new(YahooRandomPolicy$new(),                  bandit, name = "Random"))

# Define the simulation --------------------------------------------------------------------------------------

simulation <- Simulator$new(
    agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    worker_max = worker_max,
    reindex = TRUE,
    progress_file = TRUE,
    include_packages = c("MonetDB.R"))

history  <- simulation$run()

# Take a look at the results ---------------------------------------------------------------------------------

history$save_data_table(save_file_name)

print(history$meta$sim_total_duration)

plot(history, regret = FALSE, rate = TRUE, type = "cumulative", ylim = c(0.01,0.06))

dbDisconnect(con)

# Notify that sim has completed ------------------------------------------------------------------------------

pbPost("note", title = paste0("Contextual simulation complete in ",history$meta$sim_total_duration),
       body  = paste0("Saved to ",save_file_name))

