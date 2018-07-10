library(readr)
library(DBI)
library(RPostgreSQL)

# library("future.apply")
# run future_lapply in parallel on local computer
# plan(multiprocess)

# Import of the R6A - Yahoo! Front Page Today Module User Click Log Dataset

# The data_import_directory has to contain unpacked data files, themselves again unpacked from a tarball
# available by request at https://webscope.sandbox.yahoo.com/catalog.php?datatype=r&did=49

# the ydata-fp-td-clicks-v1_0.20090501 and ydata-fp-td-clicks-v1_0.20090502 files contain anomalies,
# and should NOT be included in the data_import_directory directory.

# The import generates warnings, and the readr status bar behaves irregular. This can be ignored.

# Configuration ----------------------------------------------------------------------------------------------

data_import_directory  <- "D:/Cloudy/DropBox/Dropbox/yahoo/R6A/_unpacked/"

drv         <- dbDriver('PostgreSQL')
db          <- 'yahoo'
host_db     <- 'localhost'
db_port     <- '5432'
db_user     <- 'postgres'
db_password <- 'pg'

row_max                <- 5600000 # > nr of rows in any imported file
by_step                <- 800000  # read data and write sql in in batches

# Data types -------------------------------------------------------------------------------------------------

dtypes <-
  c(
    "numeric",
    "factor",
    "factor",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    rep(
      c(
        "factor",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric"
      ),
      25
    )
  )

# Function to generate header labels -------------------------------------------------------------------------

print_headers <- function() {
  feature_gen <- function(s) {
    sapply(c(2:6, 1), FUN = function(i) paste(s, i, sep = ""))
  }
  feat_vec <- vector()
  for (i in 1:25) {
    feat_vec <- c(feat_vec, paste("a", i, "_id", sep = ""))
    feat_vec <- c(feat_vec, feature_gen(paste("a", i, "_feat", sep = "")))
  }
  c(
    c("timestamped", "article_id", "click", "delete_me"),
    c(feature_gen("user_feat"), feat_vec)
  )
}

# Convert columns to types -----------------------------------------------------------------------------------

convert_to_types <- function(obj,types){
  out <- lapply(1:length(obj),
                FUN = function(i){FUN1 <- switch(types[i],character = as.character, numeric = as.numeric,
                                                 factor = as.factor); FUN1(obj[,i])})

  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}

# connect to db ----------------------------------------------------------------------------------------------

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname=db, host=host_db, port=db_port, user=db_user, password=db_password)

# loop over each file in steps (batches) of a million records ------------------------------------------------

files = list.files(path = data_import_directory)

for (f in 1:length(files)) {
  data_file_path <- paste0(data_import_directory,files[f])

  message (paste0("\n## Starting import of ",files[f],"\n"))

  for (i in seq(0, row_max, by = by_step)) {

    # read a million records
    dat <- read_delim(data_file_path, delim = " ", col_names = print_headers(),
                      skip = i, n_max = by_step, guess_max = 1)

    message (paste0("\n## Cleaning row ",format((i+1),scientific = F),
                    " to ",format(i+by_step,scientific = F),"\n"))

    # cleanup before saving batch
    dat$delete_me <- NULL
    dat <- lapply(dat, gsub, pattern = '\\|', replacement = '', perl = TRUE)       # or future_lapply
    dat <- lapply(dat, gsub, pattern = '[1-9]:', replacement = '', perl = TRUE)    # or future_lapply
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
    dat <- convert_to_types(dat,dtypes)

    # add an index for each time step t
    if ( dim(dat)[1] >= 1 ) dat$t = seq(i + 1, i + dim(dat)[1])

    message (paste0("\n## Writing row ",format((i+1),scientific = F),
                    " to ",format(i+by_step,scientific = F),"\n"))

    # save batch (and if already data, append) to DB table "yahoo"
    dbWriteTable(con, "yahoo", dat, append = TRUE, overwrite=FALSE, row.names=FALSE)

    message (paste0("\n## On to next batch... ",format((i+1),scientific = F),
                    " to ",format(i+by_step,scientific = F),"\n"))

  }
  rm(dat)
  gc()
  message (paste0("\n## Completed import of ",files[f],"\n"))

}


dbSendQuery(con, "CREATE INDEX index_t ON yahoo (t)" )

# disconnect from and then shutdown  -------------------------------------------------------------------------

