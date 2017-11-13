library(R6)

#' @export
OfflineEvaluation <- R6Class(
  "OfflineEvaluation",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    initialize = function(policy = NA, context_arms_matrix = NA, num_sims = NA, horizon = NA, feature_vector = NA) {

    },
    run = function() {
    }
  )
)

