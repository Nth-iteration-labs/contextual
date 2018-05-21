setwd("~/GitHub/contextual/demo/dependent_observations")
source("../dev.R")

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
        X = matrix(1,self$d,self$k) # no context
      )
      contextlist
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(private$S$reward[[index]])
      if (private$S$choice[[index]] == action$choice) {
        list(reward = reward_at_index)
      } else {
        NULL
      }
    }
  )
)

library(data.table)

data_dir       <- "data/"
website_data   <- data.table::fread(paste0(data_dir,"persuasion.csv"))
horizon        <- 350000L
simulations    <- 100L
bandit         <- BasicLiBandit$new(website_data, arms = 4) # four types of webpages can be presented
agent          <- Agent$new(UCB1Policy$new(), bandit)       #
history        <- Simulator$new(agent, horizon, simulations, reindex_t = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, ylim = c(0.0105,0.015))
plot(history, type = "arms")
