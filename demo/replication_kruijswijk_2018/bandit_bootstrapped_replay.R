#' @export
DependentObservationsBootstrappedBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    x = NULL,
    rows = NULL
  ),
  public = list(
    class_name = "DependentObservationsBootstrappedBandit",
    arm_multiply = NULL,
    initialize   = function(offline_data, arms) {
      self$k <- arms
      self$d <- 1
      private$S <- offline_data
      if(!"context" %in% colnames(private$S)) private$S$context = list(1)
      private$S[is.null(context[[1]]),`:=`(context = list(1))]
      self$arm_multiply <- TRUE
      private$S <- do.call("rbind", replicate(self$k, private$S, simplify = FALSE))
      private$rows <- nrow(private$S)  # <- daar
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S),replace=TRUE)]
      private$x <- as.matrix(private$S$context)
      private$x <- apply(private$x, 2, jitter)
    },
    get_context = function(index) {
      print(index)
      if(index > private$rows) return(NULL)   # <- en daar ...
      context <- list(
        k      = self$k,
        d      = self$d,
        user_context = private$S$user[[index]],
        X      = private$x[[index]]
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
