library(DBI)
library(RSQLite)
library(ggplot2)

db_dir_file <- "D:/YahooDb/yahoo.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_dir_file)

print(dbListTables(con))

# get column names
data <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t = 1002")
sapply(data, class)
names <- names(data)
print(names)

# retrieve number of steps
nr <- dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" )
print(nr)

# get column names
data <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t > 1002 and t < 10010")
sapply(data, class)
names <- names(data)
print(names)

dt <- as.data.frame(data)


# disconnect from and then shutdown DB
dbDisconnect(con, shutdown = TRUE)


# unused looping option --------------------------------------------------------------------------------------

# query <- dbSendQuery(con, 'SELECT * FROM yahoo')
# while (!dbHasCompleted(query)) {
#   one_row <- dbFetch(query, n = 1)
#   print(one_row)
# }
# dbClearResult(query)

# ------------------------------------------------------------------------------------------------------------
