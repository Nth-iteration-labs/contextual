library(readr)
library(DBI)
library(RSQLite)

# set directory and db
db_dir_file <- "D:/YahooDb/yahoo.sqlite"

# set to previously extracted CSV file
data_day_1 <- "D:/Cloudy/DropBox/Dropbox/yahoo/R6A/unpacked/ydata-fp-td-clicks-v1_0.20090501"

# data types
dtypes <- c("numeric", "factor", "factor", "numeric", "numeric",
            "numeric", "numeric", "numeric", "numeric",
            rep(c("factor", "numeric", "numeric", "numeric",
            "numeric", "numeric", "numeric"), 22), "factor", "numeric",
            "factor", "numeric", "numeric", "numeric", "numeric",
            "numeric", "numeric")

# function to generate header labels
print_headers <- function() {
  feature_gen <- function(s) {
    sapply(c(2:6, 1), FUN = function(i) paste(s, i, sep = ""))
  }
  feat_vec <- vector()
  for (i in 1:22) {
    feat_vec <- c(feat_vec, paste("a", i, "_id", sep = ""))
    feat_vec <- c(feat_vec, feature_gen(paste("a", i, "_feat", sep = "")))
  }
  c(c("timestamped", "article_id", "click", "delete_me"),
    c(feature_gen("user_feat"), feat_vec),
    c("a24_id", "a24_feat7", "a25_id", feature_gen("a25_feat")))
}

# convert columns to types
convert_to_types <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}

# connect to db
con <- dbConnect(RSQLite::SQLite(), db_dir_file)


dbGetQuery(con,"DROP INDEX index_t ON yahoo") # drop index ... add again when inserts completed

row_max <- 5000000
by_step <- 1000000

# loop over file in steps (batches) of a million records
for (i in seq(0, row_max, by = by_step)) {

  # read a million records
  dat <- read_delim(data_day_1, delim = " ", col_names = print_headers(),
                    skip = i, n_max = by_step, guess_max = 1)

  # cleanup before saving batch
  dat$delete_me <- NULL
  dat <- lapply(dat, gsub, pattern = '\\|', replacement = '', perl = TRUE)
  dat <- lapply(dat, gsub, pattern = '[1-9]:', replacement = '', perl = TRUE)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  dat <- convert_to_types(dat,dtypes)

  # add an index for each time step t
  if ( dim(dat)[1] >= 1 ) dat$t = seq(i + 1, i + dim(dat)[1])

  # save batch (and if already data, append) to DB table "yahoo"
  dbWriteTable(con, "yahoo", dat, append = TRUE)
}

dbGetQuery(con,"CREATE INDEX index_t ON yahoo (t)") # create index on completion table .. remove when add again

nr <- dbGetQuery(con, "SELECT COUNT(*) FROM yahoo" )

# disconnect from and then shutdown
dbDisconnect(con, shutdown = TRUE)
