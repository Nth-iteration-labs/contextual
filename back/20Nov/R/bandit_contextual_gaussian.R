library(R6)


#' @export
ContextualGaussianBandit <- R6Class(

  "ContextualGaussianBandit",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    k = NULL,
    mu = NULL,
    sigma = NULL,
    action_values = NULL,
    optimal = NULL,
    contexts = NULL,

    initialize = function(contexts) {

      self$contexts = contexts
      self$reset()
    },
    reset = function() {
      self$optimal = 0
    },
    get_reward = function(action, context) {
      reward = setNames(
          list( context$click , action == context$arm, action),      ####### hmmm. . so here the reward is if clicked ... how we know optimal?
                                                              ########## this has to be decoupled between agent and bandit
          c("reward", "is_optimal", "arm")
      )
      return(reward)
    },
    get_context = function() {
      return(self$contexts$get_context())  ##heere? or in environment..
    }

  )
)
