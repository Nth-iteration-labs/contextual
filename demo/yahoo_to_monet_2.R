library(DBI)
library(RSQLite)

# set directory and db
db_dir_file <- "D:/YahooDb/yahoo.sqlite"

# connect to db
con <- dbConnect(RSQLite::SQLite(), db_dir_file)

# get current db name
print(dbListTables(con))

# get column names
data <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t = 1002")
sapply(data, class)
names <- names(data)
print(names)

# retrieve count
nr <- dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" )
print(count)

# disconnect from and then shutdown DB
dbDisconnect(con, shutdown = TRUE)

# a looping option... prefer query on indexed t for now
#
# query <- dbSendQuery(con, 'SELECT * FROM yahoo')
# while (!dbHasCompleted(query)) {
#   one_row <- dbFetch(query, n = 1)
#   print(one_row)
# }
# dbClearResult(query)
