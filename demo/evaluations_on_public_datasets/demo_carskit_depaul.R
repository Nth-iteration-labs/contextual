library(contextual)
library(data.table)

# Import personalization data-set

# Info: https://d1ie9wlkzugsxr.cloudfront.net/data_irecsys_CARSKit/Movie_DePaulMovie/README.txt

url         <- "http://d1ie9wlkzugsxr.cloudfront.net/data_irecsys_CARSKit/Movie_DePaulMovie/ratings.csv"
datafile    <- fread(url, stringsAsFactors=TRUE)

# Convert datafile

datafile    <- one_hot(datafile, cols = c("Time","Location","Companion"), sparsifyNAs = TRUE)

datafile[, userid := as.numeric(as.factor(userid))]
datafile[, itemid := as.numeric(itemid)]

datafile[, context := as.list(as.data.frame(t(datafile[, c(4:10,1)])))]
datafile[, (4:10) := NULL]

datafile[, rating := ifelse(rating <= 3, 0, 1)]

datafile[, t := .I]
datafile[, sim := 1]
datafile[, agent := "linucb"]

setnames(datafile, c("itemid", "rating"), c("choice", "reward"))

# Set simulation parameters.
simulations <- 1
horizon     <- nrow(datafile)

# Initiate Replay bandit with 10 arms and 100 context dimensions
log_S       <- datafile
bandit      <- OfflineReplayEvaluatorBandit$new(log_S, k = length(unique(datafile$choice)),
                                                       d = length(datafile$context[[1]]))

# Define agents.
agents      <-
  list(Agent$new(UCB1Policy$new(), bandit, "UCB1"),
       Agent$new(ContextualEpsilonGreedyPolicy$new(0.01), bandit, "cEG = 0.01"),
       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01), bandit, "alpha = 0.01"),
       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit, "alpha = 0.05"))

# Initialize the simulation.
simulation  <-
  Simulator$new(
    agents           = agents,
    simulations      = simulations,
    horizon          = horizon,
    save_context     = TRUE
  )

# Run the simulation.
linucb_sim  <- simulation$run()

# plot the results
plot(linucb_sim, type = "cumulative", regret = FALSE,
     rate = TRUE, legend_position = "bottomright")
