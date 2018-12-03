library(data.table)
library(contextual)
library(rstan)
library(here)

setwd(here("demo","replication_kruijswijk_2018"))

source("./bandit_replay.R")
source("./policy_pooled_egreedy.R")
source("./policy_pooled_thompson.R")
source("./policy_pooled_ucb.R")

# Import Movielens 100k --------------------------------------------------------------------------------------

# Info: https://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-100k/ml-100k-README.txt

movies_dat      <- "http://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-100k/u.item"
ratings_dat     <- "http://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-100k/u.data"
user_dat        <- "http://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-100k/u.user"

# Import and merge files

movies_dat      <- fread(movies_dat, sep = "|", quote="")
setnames(movies_dat, c("V1", "V2"), c("MovieID", "Name"))
movies_dat[, (3:5)  := NULL ]
ratings_dat     <- fread(ratings_dat, quote="")
setnames(ratings_dat, c("V1", "V2", "V3", "V4"), c("UserID", "MovieID", "Rating", "Timestamp"))
all_movies      <- ratings_dat[movies_dat, on=c(MovieID = "MovieID")]
user_dat        <- fread(user_dat, quote="")
setnames(user_dat, c("V1", "V2", "V3", "V4", "V5"),  c("UserID", "age", "gender", "occupation", "zip"))
all_movies      <- all_movies[user_dat, on=c(UserID = "UserID")]

# Select a subset of categories that are likely to have a slightly different public each
all_movies[V20 == 1, choice := 1]  # Romance
all_movies[V11 == 1, choice := 2]  # Comedy
all_movies[V14 == 1, choice := 3]  # Drama

all_movies <- all_movies[!is.na(choice)]

# Data wrangling ---------------------------------------------------------------------------------------------

# Remove top voters that vote on most types of movies
count_users     <- all_movies[,.(UserCount = .N), by = UserID]
top_users       <- as.vector(count_users[order(-UserCount)][300:nrow(count_users)]$UserID)
all_movies      <- all_movies[UserID %in% top_users]

all_movies[, t := .I]
all_movies[, sim := 1]
all_movies[, agent := "Offline"]

# A rating of 3 or higher is rewarded with 1, otherwise 0
all_movies[, reward := ifelse(Rating <= 3, 0, 1)]

all_movies[, user := as.numeric(as.factor(UserID))]

usercount <- length(unique(all_movies$user))
armcount  <- length(unique(all_movies$choice))

# Run simulation ---------------------------------------------------------------------------------------------

horizon     <- nrow(all_movies)
simulations <- 5
bandit      <- DependentObservationsReplayBandit$new(all_movies , armcount)

agents      <- list(Agent$new(UnpooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = usercount), bandit),
                    Agent$new(PooledEgreedyPolicy$new(epsilon = 0.1), bandit),
                    Agent$new(RandomPolicy$new(), bandit),
                    Agent$new(PartiallyPooledBBEgreedyPolicy$new(epsilon = 0.1, n_subjects = usercount), bandit),
                    Agent$new(PartiallyPooledEgreedyPolicy$new(epsilon = 0.1, n_subjects = usercount), bandit))

history     <- Simulator$new(agents, horizon, simulations, save_interval = 50)$run()

plot(history,type = "cumulative", regret = FALSE, legend_border = FALSE,
     rate = TRUE, legend_position = "bottomright", ylim = c(0.55,0.72))



