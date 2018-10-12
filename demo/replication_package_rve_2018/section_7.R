library(contextual)
library(data.table)
library(here)

setwd(here::here("demo","replication_package_rve_2018"))

OfflineReplayEvaluatorBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "OfflineReplayEvaluatorBandit",
    randomize = NULL,
    initialize   = function(data_stream, k, d) {
      self$k <- k               # Number of arms (integer)
      self$d <- d               # Dimension context features (integer)
      private$S <- data_stream  # Data stream, as a data.table
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S))]
    },
    get_context = function(index) {
      context <- list(
        k = self$k,
        d = self$d,
        X = matrix(private$S$daypart[[index]], self$d, self$k)
      )
      context
    },
    get_reward = function(index, context, action) {
      reward           <- as.double(private$S$reward[[index]])
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = reward
        )
      } else {
        NULL
      }
    }
  )
)

data_url <- "https://raw.githubusercontent.com"
data_url <- paste0(data_url,"/Nth-iteration-labs/contextual_data/master")
data_url <- paste0(data_url,"/data_persuasion_api/persuasion_api_daypart.csv")

data     <- fread(data_url)

horizon  <- nrow(data)
sims     <- 10L
bandit   <- OfflineReplayEvaluatorBandit$new(data, k = 4, d = 1)
agents   <- list(Agent$new(LinUCBHybridPolicy$new(0.6), bandit))

history  <- Simulator$new(agents, horizon, sims, reindex = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE, smooth = TRUE,
     traces = TRUE, rate = TRUE, ylim = c(0.0105, 0.014), legend = FALSE)
