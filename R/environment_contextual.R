library(R6)


#' @export
MultiBanditEnv <- R6Class(

  "MultiBanditEnv",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  private = list(
    rewards = NULL
  ),
  public = list(

    iterations = NULL,
    epochs = NULL,
    m_bandits = NULL,

    initialize = function(iterations = NA, epochs = NA) {

      self$iterations = iterations
      self$epochs = epochs
      private$rewards = data.frame()
    },
    get_rewards = function() {
      return(private$rewards)
    },

    make_env = function(bandit) {
      self$m_bandits = MultiBandits()
      for (bandit in bandits) {
        self$m_bandits.add_bandit(bandit)
      }
    },

    reset_env = function(self){
      for (bandit in self$m_bandits$bandits) {
        bandit.reset()
      }
    },

    run = function(agent){
      warning('Not implemented error.')
    }

  )
)
