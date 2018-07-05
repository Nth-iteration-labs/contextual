library(DBI)
library(RSQLite)
library(ggplot2)

db_dir_file <- "D:/YahooDb/yahoo.sqlite"

con <- dbConnect(RSQLite::SQLite(), db_dir_file)

print(dbListTables(con))


# get all articles
for (i in 1:25) {
  if(i!=23) {
    query_res <- dbGetQuery(con, paste0("SELECT DISTINCT a",i,"_id FROM yahoo"))
    if (i==1) {
      results <- query_res
    } else {
      results <- rbind(results, setNames(rev(query_res), names(results)))
    }
  }
}
results <- na.omit(results)


# Traffic ----------------------------------------------------------------------------------------------------

res <- dbGetQuery(con,"SELECT timestamped, COUNT(timestamped) FROM yahoo GROUP BY timestamped")
times <- dbFetch(res, n = -1)
names(times) <- c('timestamped', 'count')
times$timestamped <- as.POSIXct(times$timestamped, origin = "1970-01-01")
ggplot(times, aes(timestamped, count)) + geom_line() + ggtitle("Traffic")

# CTR --------------------------------------------------------------------------------------------------------

res <-
  dbGetQuery(con,
              "SELECT timestamped, AVG(click) FROM yahoo GROUP BY timestamped")
ctr <- dbFetch(res, n = -1)
names(ctr) <- c('timestamped', 'ctr')
ctr$timestamped <- as.POSIXct(ctr$timestamped, origin = "1970-01-01")
ggplot(ctr, aes(timestamped, ctr)) + geom_line() + ggtitle("CTR")

# Articles with best CTRs ------------------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    "SELECT article_id, AVG(click) FROM yahoo LEFT JOIN article ON yahoo.displayed=article.article_id GROUP BY article_id ORDER BY AVG(click) DESC"
  )
best_articles <- dbFetch(res, n = -1)

# Favorite article by cluster --------------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    "SELECT cluster, article_id, AVG(click) from yahoo LEFT JOIN article
    ON yahoo.displayed=article.article_id
    LEFT JOIN user ON yahoo.userID=user.userID GROUP BY cluster"
  )

cluster_favorites <- dbFetch(res, n = -1)

# Cluster clickthrough rates ---------------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    "SELECT cluster, AVG(click), timestamped from yahoo
    LEFT JOIN user ON yahoo.userID=user.userID GROUP BY cluster, timestamped"
  )

ctr_by_cluster <- dbFetch(res, n = -1)
names(ctr_by_cluster) <- c('cluster', 'ctr', 'timestamped')
ctr_by_cluster$timestamped <-
  as.POSIXct(ctr_by_cluster$timestamped, origin = "1970-01-01")
ggplot(ctr_by_cluster, aes(x = timestamped, y = ctr, colour = cluster)) + geom_line() +
  stat_smooth(method = 'loess',
              formula = y ~ x,
              size = 1)

# Best arm per time period -----------------------------------------------------------------------------------

res <- dbGetQuery(
  con,
  "SELECT MAX(ctr), timestamped, article_id
  from (SELECT AVG(click) as ctr, timestamped, article_id
  from yahoo LEFT JOIN article ON yahoo.displayed=article.article_id GROUP
  BY timestamped, article_id) GROUP BY timestamped"
)

arm_ctr <- dbFetch(res, n = -1)
names(arm_ctr)[1] <- c('ctr')
arm_ctr$timestamped <-
  as.POSIXct(arm_ctr$timestamped,  origin = "1970-01-01")
ggplot(arm_ctr, aes(
  x = timestamped,
  y = ctr,
  fill = as.factor(article_id)
)) + geom_bar(stat = 'Identity')

# Best arm per time period by cluster ------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    "SELECT MAX(ctr), timestamped, article_id, cluster
    from (SELECT AVG(click) as ctr, timestamped, article_id, cluster from yahoo
    LEFT JOIN article ON yahoo.displayed=article.article_id LEFT JOIN user
    ON yahoo.userID=user.userID GROUP BY timestamped, article_id, cluster)
    GROUP BY timestamped, cluster"
  )

arm_ctr_by_cluster <- dbFetch(res, n = -1)
names(arm_ctr_by_cluster)[1] <- c('ctr')
arm_ctr_by_cluster$timestamped <-
  as.POSIXct(arm_ctr_by_cluster$timestamped,  origin = "1970-01-01")
ggplot(arm_ctr_by_cluster,
       aes(
         x = timestamped,
         y = ctr,
         fill = as.factor(article_id)
       )) + geom_bar(stat = 'Identity') +
  facet_wrap( ~ cluster)

# Top 5 arms per cluster -------------------------------------------------------------------------------------

top_arms_by_cluster <- data.frame()
for (i in 2:6) {
  res <-
    dbGetQuery(
      con,
      paste(
        "SELECT AVG(click) as ctr, article_id, cluster from yahoo
        LEFT JOIN article ON yahoo.displayed=article.article_id
        LEFT JOIN user ON yahoo.userID=user.userID WHERE cluster=",
        i,
        "GROUP BY article_id ORDER BY ctr DESC LIMIT 5"
      )
    )
  top_arms_by_cluster <-
    rbind(top_arms_by_cluster, dbFetch(res, n = -1))
}
ggplot(top_arms_by_cluster, aes(
  x = article_id,
  y = ctr,
  fill = as.factor(article_id)
)) +
  geom_bar(stat = 'Identity') + facet_wrap( ~ cluster, ncol = 1)

# get ctr for specific article -------------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    'SELECT AVG(click) FROM yahoo LEFT JOIN user ON yahoo.userID=user.userID
    WHERE yahoo.displayed=109453 AND user.cluster=2'
  )
dbFetch(res, n = -1)

# clickthrough rates, cluster agnostic -----------------------------------------------------------------------

res <-
  dbGetQuery(con,
              'SELECT AVG(click) as ctr, displayed as article_id from yahoo GROUP BY displayed')
ctrs <- dbFetch(res, n = -1)

# ctr by cluster ---------------------------------------------------------------------------------------------

res <-
  dbGetQuery(
    con,
    'SELECT AVG(click) as ctr, article_id, cluster from yahoo LEFT JOIN article
    ON yahoo.displayed=article.article_id LEFT JOIN user ON yahoo.userID=user.userID
    GROUP BY article_id, cluster'
  )
ctrs_cluster <- dbFetch(res, n = -1)

# max ID -----------------------------------------------------------------------------------------------------

res <- dbGetQuery(con, 'SELECT MAX(yahooID) from yahoo')
dbFetch(res)

# Bin Value of Max Feature -----------------------------------------------------------------------------------

# Return the max feature for each user. Histogram bin the occurances
res <-
  dbGetQuery(con,
              'SELECT MAX(feat2, feat3, feat4, feat5, feat6) FROM user GROUP BY userID')
max_feats <- dbFetch(res, n = -1)

# disconnect from and then shutdown DB -----------------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)
