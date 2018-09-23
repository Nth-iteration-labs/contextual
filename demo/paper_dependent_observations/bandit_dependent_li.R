#' @export
DependentObservationsLiBandit <- R6::R6Class(
  "DependentObservationsLiBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    initialize   = function(offline_data, arms) {
      self$k <- arms
      self$d <- 1
      private$S <- offline_data
    },
    get_context = function(index) {
      contextlist <- list(
        k = self$k,
        d = self$d,
        user_context = private$S$user[[index]]
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
