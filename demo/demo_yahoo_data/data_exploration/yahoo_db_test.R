library(DBI)
library(MonetDBLite)
library(MonetDB.R)

#db_dir <- "C:/YahooDb/yahoo.monetdblite"
#con    <- dbConnect(MonetDBLite::MonetDBLite(), db_dir)
con <- DBI::dbConnect(MonetDB.R(), host="localhost", dbname="yahoo", user="monetdb", password="monetdb")

print(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Test loop  -------------------------------------------------------------------------------------------------

for (i in 1:300) {
  result <- dbGetQuery(con, paste0("SELECT * FROM yahoo WHERE t = ", (i), " LIMIT 1") )
  print(result$a1_id)
  print(i)
}

# Test names  ------------------------------------------------------------------------------------------------

ok <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t = '724785' LIMIT 1" )

wrong_1 <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t = '724786' LIMIT 1" )

wrong_2 <- dbGetQuery(con, "SELECT * FROM yahoo WHERE t = '724787' LIMIT 1" )




# Max t

max_t <- dbGetQuery(con, "SELECT max(t) FROM yahoo" )


# disconnect from and then shutdown DB
dbDisconnect(con)
