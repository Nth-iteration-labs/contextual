
setwd("~/GitHub/contextual/demo/dependent_observations")
source("../dev.R")

library(data.table)

BasicLiBandit <- R6::R6Class(
  "BasicLiBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    initialize   = function(data_stream, arms) {
      self$k <- arms
      self$d <- 1
      private$S <- data_stream
    },
    get_context = function(index) {
      contextlist <- list(
        k = self$k,
        d = self$d,
        # A very basic context:
        # the user's id as a feature over both arms.
        X = matrix(private$S$user[[index]],self$d,self$k)
      )
      contextlist
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(private$S$reward[[index]])
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = reward_at_index
        )
      } else {
        NULL
      }
    }
  )
)

horizon        <- 350000L
simulations    <- 1000L

data_dir       <- "data/"
data           <- data.table::fread(paste0(data_dir,"persuasion.csv"))

bandit         <- BasicLiBandit$new(data, 4)

agents         <- list( Agent$new(EpsilonGreedyPolicy$new(epsilon = 0.1), bandit),
                        Agent$new(LinUCBDisjointPolicy$new(1.0, "LinUCB"), bandit),
                        Agent$new(UCB1Policy$new(), bandit))

history        <- Simulator$new(agents, horizon, simulations, reindex_t = TRUE)$run()

history$save_data(paste0(data_dir,"persuasion_simulation.RData"))

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.0105,0.015))
