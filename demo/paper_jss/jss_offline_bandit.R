library(contextual)
library(data.table)
library(RCurl)
library(foreign)

LiSamplingOfflineBandit <- R6::R6Class(
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "LiSamplingOfflineBandit",
    randomize = NULL,
    initialize   = function(data_stream, k, d) {
      self$k <- k               # Number of arms (integer)
      self$d <- d               # Dimension of context feature vector (integer)
      private$S <- data_stream  # Data stream, as a data.table
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S))]
    },
    get_context = function(index) {
      contextlist <- list(
        k = self$k,
        d = self$d,
        X = matrix(private$S$daypart, self$d, self$k)
      )
      contextlist
    },
    get_reward = function(index, context, action) {
      reward_at_index  <- as.double(private$S$reward[[index]])
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

#url <- "https://raw.githubusercontent.com/Nth-iteration-labs/contextual_data/"
#url <- paste0(url,"master/data_persuasion_api/persuasion_api_daypart.csv")
#website_data   <- getURL(setDT(read.csv(textConnection(website_data))))

website_data   <- setDT(read.csv("persuasion_api_daypart-2.csv"))

horizon        <- nrow(website_data)
simulations    <- 10L
bandit         <- LiSamplingOfflineBandit$new(website_data, k = 4, d = 1)
agents         <- list(#Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit),
                       Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit)
                       #Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit)
                       )

history        <- Simulator$new(agents, horizon, simulations, reindex = TRUE)$run()

plot(history, type = "cumulative", regret = FALSE, smooth = TRUE, traces = TRUE, traces_alpha = 0.7,
     rate = TRUE, ylim = c(0.0105, 0.014), legend_position = "topright")

