#' @export
OnlineOfflineContinuumBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "OnlineOfflineContinuumBandit",
    delta = NULL,
    c1 = NULL,
    c2 = NULL,
    horizon = NULL,
    choice = NULL,
    initialize   = function(delta, horizon) {
      self$horizon <- horizon
      self$delta <- delta
      self$k <- 1
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S)),]
      self$c1 <- runif(1,0.25,0.75)
      self$c2 <- 1 #runif(1,0.25,0.75)
      self$choice <- runif(self$horizon, min=0, max=1)
      private$S <- data.frame(self$choice, self$arm_function(self$choice, self$c1, self$c2))
      colnames(private$S) <- c('choice', 'reward')
    },
    arm_function = function(x, c1 = 0.25, c2 = 0.75) {
      -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
    },
    get_context = function(index) {
      context           <- list()
      context$k         <- self$k
      context
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(private$S$reward[[index]])
      if (abs(private$S$choice[[index]] - action$choice) < self$delta) {
        reward <- list(
          reward = reward_at_index,
          optimal_reward = self$c2
        )
      } else {
        NULL
      }
    }
  )
)
