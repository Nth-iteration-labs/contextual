library(DBI)
library(MonetDBLite)
library(here)
library(data.table)
library(MonetDB.R)

setwd(here::here("demo", "replication_li_2010", "demo_yahoo_exploration"))

con <- DBI::dbConnect(MonetDB.R(), host="monetdb_ip", dbname="yahoo", user="monetdb", password="monetdb")

print(paste0("MonetDB: connection to '",dbListTables(con),"' database succesful!"))

# Rows -------------------------------------------------------------------------------------------------------

if(!file.exists("../demo_yahoo_cache/rows.Rds")){
  rows <- as.integer(dbGetQuery(con, "SELECT COUNT(a1_id) FROM yahoo" ))
  saveRDS(rows, file = "../demo_yahoo_cache/rows.Rds")
} else {
  if(!exists("rows")) rows <- readRDS(file = "../demo_yahoo_cache/rows.Rds")
}

# Clusters of articles ---------------------------------------------------------------------------------------

if(!file.exists("../demo_yahoo_cache/clusters_articles_df.Rds")){
  clusters_articles <- as.data.table(dbGetQuery(con, paste0("SELECT DISTINCT ",
                                  paste0("a",1:24,"_id,",collapse=""),
                                  " a25_id FROM yahoo")))
  clusters_articles[, cluster_id := .I]
  setindex(clusters_articles,cluster_id)
  saveRDS(clusters_articles, file = "../demo_yahoo_cache/clusters_articles_df.Rds")
} else {
  if(!exists("clusters_articles_df")) clusters_articles <- readRDS(file = "../demo_yahoo_cache/clusters_articles_df.Rds")
}

# All article vector -----------------------------------------------------------------------------------------

if(!file.exists("../demo_yahoo_cache/articles_vector.Rds")){
  articles_vector <- unique(as.vector(as.matrix(dbGetQuery(con, paste0("SELECT DISTINCT ",
                                                            paste0("a",1:24,"_id,",collapse=""),
                                                            " a25_id FROM yahoo")))))
  articles_vector <- as.integer(na.omit(articles_vector))
  saveRDS(articles_vector, file = "../demo_yahoo_cache/articles_vector.Rds")
} else {
  if(!exists("articles_vector")) articles_vector <- readRDS(file = "../demo_yahoo_cache/articles_vector.Rds")
}

# Articles vs Features matrix --------------------------------------------------------------------------------

if(file.exists("../demo_yahoo_cache/articles_features_matrix.Rds")){
  first = TRUE
  for(article in articles_vector){
    row <- dbGetQuery(con, paste0("SELECT * FROM yahoo WHERE article_id='",article,"' LIMIT 1"))
    start_col_article <- which(row[1,3:185] == article) + 3
    article_features <- as.matrix(row[1,start_col_article:(start_col_article+5)])
    colnames(article_features) <- NULL
    if (first) {
      articles_features_matrix <- article_features
      first <- FALSE
    } else {
      articles_features_matrix <- rbind(articles_features_matrix,article_features)
    }
  }
  rm(article_features,row,start_col_article,first)
  rownames(articles_features_matrix) <- articles_vector
  saveRDS(articles_features_matrix, file = "../demo_yahoo_cache/articles_features_matrix.Rds")
} else {
  if(!exists("articles_features_matrix"))
    articles_features_matrix <- readRDS(file = "../demo_yahoo_cache/articles_features_matrix.Rds")
}

# disconnect from and then shutdown DB -----------------------------------------------------------------------


dbDisconnect(con)

