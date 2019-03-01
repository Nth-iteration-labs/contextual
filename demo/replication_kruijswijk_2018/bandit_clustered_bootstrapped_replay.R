#' @export
library(plyr)
DependentObservationsClusteredBootstrappedBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    x = NULL,
    nusers = NULL,
    nrows = NULL,
    mean_count = NULL,
    sample_users = NULL
  ),
  public = list(
    class_name = "DependentObservationsClusteredBootstrappedBandit",
    arm_multiply = NULL,
    initialize   = function(offline_data, arms) {
      self$k <- arms
      self$d <- 1
      private$S <- offline_data
      if(!"context" %in% colnames(private$S)) private$S$context = list(1)
      private$S[is.null(context[[1]]),`:=`(context = list(1))]

      self$arm_multiply <- TRUE   # <---- something off here..
      private$nusers <- length(unique(private$S$user))
      private$nrows <- nrow(private$S) * self$k
      private$mean_count <- mean(plyr::count(private$S,'user')[['freq']])
      private$sample_users <- self$k*private$nrows/private$mean_count
    },
    post_initialization = function() {
      message("started post-initialization")
      new_user_combination <- sample(private$nusers, size = private$sample_users, replace = TRUE)
      user_id = 1
      new_S = c()
      setkey(private$S,user)
      for(one_user in new_user_combination){
        user_sample <- private$S[user==one_user]
        user_sample$user <- user_id
        new_S <- rbindlist(list(new_S, user_sample))
        user_id <- user_id + 1
      }
      private$S <- new_S
      private$S <- private$S[sample(nrow(private$S)),]
      private$S$user <- as.numeric(as.factor(private$S$user))
      message("completed post-initialization")
    },
    get_context = function(index) {
      print(111)
      print(length(private$S$user))
      context <- list(
        k      = self$k,
        d      = self$d,
        user_context = private$S$user[[index]]
      )
      print(222)
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
