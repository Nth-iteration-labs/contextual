library(DBI)
library(MonetDBLite)
library(here)
library(data.table)

setwd(here("demo", "demo_yahoo_data"))

db_dir <- "C:/YahooDb/yahoo.monetdblite"
con    <- dbConnect(MonetDBLite::MonetDBLite(), db_dir)

print(paste0("MonetDBLite: connection to '",dbListTables(con),"' database succesful!"))

# Rows -------------------------------------------------------------------------------------------------------

if(!file.exists("./cache/rows.Rds")){
  rows <- as.integer(dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" ))
  saveRDS(rows, file = "./cache/rows.Rds")
} else {
  if(!exists("rows")) rows <- readRDS(file = "./cache/rows.Rds")
}

# Arm to articles lookup -------------------------------------------------------------------------------------

if(!file.exists("./cache/arm_article.Rds")){
  arm_article <- as.data.table(dbGetQuery(con, "SELECT DISTINCT article_id FROM yahoo"))
  arm_article[, arm := .I]
  setindex(arm_article,arm)
  saveRDS(arm_article, file = "./cache/arm_article.Rds")
} else {
  if(!exists("arm_article")) arm_article <- readRDS(file = "./cache/arm_article.Rds")
}

# Clusters of articles ---------------------------------------------------------------------------------------

if(!file.exists("./cache/clusters_articles_df.Rds")){
  clusters_articles <- as.data.table(dbGetQuery(con, paste0("SELECT DISTINCT ",
                                  paste0("a",1:24,"_id,",collapse=""),
                                  " a25_id FROM yahoo")))
  clusters_articles[, cluster_id := .I]
  setindex(clusters_articles,cluster_id)
  saveRDS(clusters_articles, file = "./cache/clusters_articles_df.Rds")
} else {
  if(!exists("clusters_articles_df")) clusters_articles <- readRDS(file = "./cache/clusters_articles_df.Rds")
}

# Articles vs Features matrix --------------------------------------------------------------------------------

if(!file.exists("./cache/articles_features_matrix.Rds")){
  for(i in seq_along(arm_article$article_id)){
    row <- dbGetQuery(con, paste0("SELECT * FROM yahoo WHERE article_id='",arm_article$article_id[i],"' LIMIT 1"))
    start_col_article <- which(row[1,3:185] == arm_article$article_id[i]) + 3
    article_features <- as.matrix(row[1,start_col_article:(start_col_article+5)])
    colnames(article_features) <- NULL
    if (i>1) {
      articles_features_matrix <- rbind(articles_features_matrix,article_features)
    } else {
      articles_features_matrix <- article_features
    }
  }
  rm(article_features,row,i,start_col_article)
  rownames(articles_features_matrix) <- arm_article$article_id
  saveRDS(articles_features_matrix, file = "./cache/articles_features_matrix.Rds")
} else {
  if(!exists("articles_features_matrix"))
    articles_features_matrix <- readRDS(file = "./cache/articles_features_matrix.Rds")
}

# disconnect from and then shutdown DB -----------------------------------------------------------------------


dbDisconnect(con, shutdown = TRUE)

