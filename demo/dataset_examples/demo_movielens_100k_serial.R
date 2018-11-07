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

count_movies    <- all_movies[,.(MovieCount = .N), by = MovieID]
top_50          <- as.vector(count_movies[order(-MovieCount)][1:50]$MovieID)
not_50          <- as.vector(count_movies[order(-MovieCount)][51:nrow(count_movies)]$MovieID)

top_50_movies   <- all_movies[MovieID %in% top_50]

# User features: tags they've watched for non-top-50 movies normalized per user

user_features   <- all_movies[MovieID %in% not_50]

rm(all_movies)

user_features[, c("MovieID", "Rating", "Timestamp", "Name"):=NULL]
user_features   <- user_features[, lapply(.SD, sum, na.rm=TRUE), by=UserID ]

user_features[, total := rowSums(.SD, na.rm = TRUE), .SDcols = 2:20]

user_features[, 2:20 := lapply(.SD, function(x) x/user_features$total), .SDcols = 2:20]

user_features$total <- NULL

# Add user features to top50
top_50_movies      <- top_50_movies[user_features, on=c(UserID = "UserID")]
top_50_movies      <- na.omit(top_50_movies)

rm(user_features, not_50, top_50, count_movies)

# Parse for Contextual

top_50_movies[, context := as.list(as.data.frame(t(top_50_movies[, c(25:43)])))]

top_50_movies[, (6:43)  := NULL ]

top_50_movies[, t := .I]
top_50_movies[, sim := 1]
top_50_movies[, agent := "Offline"]
top_50_movies[, choice := as.numeric(as.factor(MovieID))]
top_50_movies[, reward := ifelse(Rating <= 4, 0, 1)]

setorder(top_50_movies,Timestamp,Name)


# Run simulation ---------------------------------------------------------------------------------------------

simulations <- 10
horizon     <- nrow(top_50_movies)

bandit      <- OfflineReplayEvaluatorBandit$new(top_50_movies, k = length(unique(top_50_movies$choice)),
                                                d = length(top_50_movies$context[[1]]),
                                                unique = c(1:19))

agents      <-
  list(Agent$new(ThompsonSamplingPolicy$new(), bandit, "Thompson"),
       Agent$new(RandomPolicy$new(), bandit, "Random"),
       Agent$new(LinUCBDisjointOptimizedPolicy$new(2.05), bandit, "LinUCB Dis"))

simulation  <-
  Simulator$new(
    agents           = agents,
    simulations      = simulations,
    horizon          = horizon,
    reindex          = TRUE
  )

linucb_sim  <- simulation$run()

plot(linucb_sim, type = "cumulative", regret = FALSE,
     rate = TRUE, legend_position = "topleft")
