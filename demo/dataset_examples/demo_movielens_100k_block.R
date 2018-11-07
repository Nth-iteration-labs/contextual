library(contextual)
library(data.table)
library(splitstackshape)

# Movielens 100k ---------------------------------------------------------------------------------------------

# Info: https://contextualdata.s3-eu-central-1.amazonaws.com/data_movielens/ml-100k/ml-100k-README.txt

movies_dat      <- "https://contextualdata.s3-eu-central-1.amazonaws.com/data_movielens/ml-100k/u.item"
ratings_dat     <- "https://contextualdata.s3-eu-central-1.amazonaws.com/data_movielens/ml-100k/u.data"

# Import and merge files

movies_dat      <- fread(movies_dat, sep = "|", quote="")
setnames(movies_dat, c("V1", "V2"), c("MovieID", "Name"))
movies_dat[, (3:5)  := NULL ]
ratings_dat     <- fread(ratings_dat, quote="")
setnames(ratings_dat, c("V1", "V2", "V3", "V4"), c("UserID", "MovieID", "Rating", "Timestamp"))
all_movies      <- ratings_dat[movies_dat, on=c(MovieID = "MovieID")]

rm(movies_dat,ratings_dat)

# Data wrangling ---------------------------------------------------------------------------------------------

all_movies[, UserID   := as.numeric(as.factor(UserID))]

count_movies    <- all_movies[,.(MovieCount = .N), by = MovieID]
top_50          <- as.vector(count_movies[order(-MovieCount)][1:50]$MovieID)
not_50          <- as.vector(count_movies[order(-MovieCount)][51:nrow(count_movies)]$MovieID)

top_50_movies   <- all_movies[MovieID %in% top_50]

# ------------------------------------------------------------------------------------------------------------

# Create feature lookup tables - to speed up, MovieID and UserID are
# ordered and lined up with the (dt/matrix) default index.

# Arm (common or shared) features

# MovieID of top 50 ordered from 1 to N:
top_50_movies[, MovieID   := as.numeric(as.factor(MovieID))]
arm_features    <- top_50_movies[,head(.SD, 1),by = MovieID][,c(1,6:24)]
setorder(arm_features,MovieID)

# User (unique) features

# Count of categories for non-top-50 movies normalized per user
user_features   <- all_movies[MovieID %in% not_50]
user_features[, c("MovieID", "Rating", "Timestamp", "Name"):=NULL]
user_features   <- user_features[, lapply(.SD, sum, na.rm=TRUE), by=UserID ]
user_features[, total  := rowSums(.SD, na.rm = TRUE), .SDcols = 2:20]
user_features[, 2:20 := lapply(.SD, function(x) x/total), .SDcols = 2:20]
user_features$total <- NULL

# Add users that were not in the set of non-top-50 movies (none in 100k dataset)
all_users <- as.data.table(unique(all_movies$UserID))
user_features <- user_features[all_users, on=c(UserID = "V1")]
user_features[is.na(user_features)] <- 0

setorder(user_features,UserID)

# ------------------------------------------------------------------------------------------------------------

rm(all_movies, not_50, top_50, count_movies)

# ------------------------------------------------------------------------------------------------------------

# Contextual format

top_50_movies[, t := .I]
top_50_movies[, sim := 1]
top_50_movies[, agent := "Offline"]
top_50_movies[, choice := MovieID]
top_50_movies[, reward := ifelse(Rating <= 4, 0, 1)]

# Run simulation ---------------------------------------------------------------------------------------------

simulations <- 10
horizon     <- nrow(top_50_movies)

bandit      <- OfflineLookupReplayEvaluatorBandit$new(top_50_movies,
                                                      k             = 50,
                                                      unique_col    = "UserID",
                                                      shared_lookup = arm_features,
                                                      unique_lookup = user_features)

agents      <-
  list(Agent$new(ThompsonSamplingPolicy$new(), bandit, "Thompson"),
       Agent$new(RandomPolicy$new(), bandit, "Random"),
       Agent$new(LinUCBHybridOptimizedPolicy$new(1.0), bandit, "LinUCB Hyb"),
       Agent$new(LinUCBDisjointOptimizedPolicy$new(2.05), bandit, "LinUCB"))

simulation  <-
  Simulator$new(
    agents           = agents,
    simulations      = simulations,
    horizon          = horizon,
    reindex          = TRUE
  )

history  <- simulation$run()

plot(history, type = "cumulative", regret = FALSE,
     rate = TRUE, legend_position = "topleft")
