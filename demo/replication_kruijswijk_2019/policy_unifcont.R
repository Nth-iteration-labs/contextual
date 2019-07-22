UniformRandomContinuousPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    class_name = "UniformRandomContinuousPolicy",
    initialize = function() {
      super$initialize()
    },
    set_parameters = function(context_params) {
    },
    get_action = function(t, context) {
      action$choice <- runif(1,min = 0, max = 1)
      action
    },
    set_reward = function(t, context, action, reward) {
      self$theta
    }
  )
)
