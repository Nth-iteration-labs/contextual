#' @export
DependentObservationsReplayBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    class_name = "DependentObservationsReplayBandit",
    initialize   = function(offline_data, arms) {
      self$k <- arms
      self$d <- 1
      private$S <- offline_data
      if(!"context" %in% colnames(private$S)) private$S$context = list(1)
      private$S[is.null(context[[1]]),`:=`(context = list(1))]
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S))]
    },
    get_context = function(index) {
      context <- list(
        k      = self$k,
        d      = self$d,
        user_context = private$S$user[[index]],
        X      = private$S$context[[index]]
      )
      context
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
