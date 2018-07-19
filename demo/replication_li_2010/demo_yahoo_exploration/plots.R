library(DBI)
library(MonetDBLite)
library(ggplot2)

con <- DBI::dbConnect(MonetDB.R(), host="monetdb_ip", dbname="yahoo", user="monetdb", password="monetdb")
print(paste0("MonetDB: connection to '",dbListTables(con),"' database succesful!"))

times <- dbGetQuery(con, "SELECT timestamped, COUNT(timestamped) FROM yahoo GROUP BY timestamped")
names(times) <- c('timestamped', 'count')
times$timestamped <- as.POSIXct(times$timestamped, origin = "1970-01-01")

# Traffic ----------------------------------------------------------------------------------------------------

times <- dbGetQuery(con, "SELECT timestamped, COUNT(timestamped) FROM yahoo GROUP BY timestamped")
names(times) <- c('timestamped', 'count')
times$timestamped <- as.POSIXct(times$timestamped, origin = "1970-01-01")
ggplot(times, aes(timestamped, count)) + geom_line() + ggtitle("Traffic")

# CTR over time ----------------------------------------------------------------------------------------------

ctr <-
  dbGetQuery(con, "SELECT timestamped, AVG(click) FROM yahoo GROUP BY timestamped")
names(ctr) <- c('timestamped', 'ctr')
ctr$timestamped <- as.POSIXct(ctr$timestamped, origin = "1970-01-01")
ggplot(ctr, aes(timestamped, ctr)) + geom_line() + ggtitle("CTR")

# clickthrough rates, no context, no cluster -----------------------------------------------------------------

ctrs <- dbGetQuery(con, 'SELECT article_id, AVG(click) as ctr  from yahoo GROUP BY article_id ORDER BY ctr')

barplot(ctrs$ctr, names.arg=ctrs$article_id, ylim=c(0,0.1))

# top 5

barplot(tail(ctrs$ctr,5), names.arg=tail(ctrs$article_id,5), ylim=c(0,0.1))

# worst 10

barplot(head(ctrs$ctr,5), names.arg=head(ctrs$article_id,5), ylim=c(0,0.1))

# disconnect from and then shutdown DB -----------------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
