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
    horizon = NULL,
    choice = NULL,
    arm_function = NULL,
    initialize   = function(FUN, delta, horizon) {
      self$arm_function <- FUN
      self$horizon <- horizon
      self$delta <- delta
      self$k <- 1
    },
    post_initialization = function() {
      self$choice <- runif(self$horizon, min=0, max=1)
      private$S <- data.frame(self$choice, self$arm_function(self$choice))
      private$S <- private$S[sample(nrow(private$S)),]
      colnames(private$S) <- c('choice', 'reward')
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
          reward = reward_at_index
        )
      } else {
        NULL
      }
    }
  )
)
