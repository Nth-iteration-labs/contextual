library(R6)

#' @export
ContextualOfflineEvaluation <- R6Class(
  "OfflineEvaluation",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    last_cummulative = 0L,

    policy = NULL,
    arms = NULL,
    num_sims = NULL,
    horizon = NULL,

    chosen_arms = NULL,
    rewards = NULL,
    cumulative_rewards = NULL,
    sim_nums = NULL,
    times = NULL,
    feature_vector = NULL,

    initialize = function(policy = NA, context_arms_matrix = NA, num_sims = NA, horizon = NA, feature_vector = NA) {
      self$policy <- policy
      self$arms <- context_arms_matrix
      self$num_sims <- num_sims
      self$horizon <- horizon
      self$feature_vector <- feature_vector

    },
    # this is all really nice and dandy, byt
    run = function() {
      sim <- 1L
      list_of_results = list()
      for (sim in 1L:length(self$feature_vector)) {
        offline = OfflineEvaluation$new(policy, arms[sim,], num_sims, horizon)
        results = offline$run()
        results$Epsilon <- rep(self$feature_vector[sim], nrow(results))
        list_of_results[[sim]] = results
      }
      df = do.call("rbind", list_of_results)
    }
  )
)



