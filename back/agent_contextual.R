library(R6)

#' @export
ContextualAgent <- R6Class(

  "ContextualAgent",

  inherit = Agent,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    k = NULL,
    d = NULL,
    prior = NULL,
    gamma = NULL,
    action_attempts = NULL,
    t = NULL,
    last_action = NULL,
    init_exploration = NULL,
    memory = list(),
    states = vector(mode = "numeric", length = 0),

    #needed to redefine these above here? no?
    #order of parameters below, also, NA everywhere?
    initialize = function(k = NA ,
                          d = NA,
                          policy = NA,
                          init_exploration = NA,
                          prior = 0,
                          gamma = NA
                          ) {
      super$initialize(policy, prior, gamma, init_exploration, k)
      self$d = d

      self$reset()
    },

    reset = function() {

      # access as: memory[[1]][['A']][1,1]
      memory_cell = list('A' = diag(1,d),'b' = matrix(0,d,1) )
      for (i in 1:k) {
        this$memory[[i]] = memory_cell
      }
      self$states = vector(mode = "numeric", length = 0)

      # can do these via super!
      private$value_estimates = rep(prior, self$k) # Est. Mean reward
      self$action_attempts = rep(0, self$k)
      self$last_action = NA
      self$t = 0
    },

    get_state = function(bandit) {
      self$states = bandit$states


      for (action in 1:NROW(this$memory)) {
        A = this$memory[[action]][['A']]
        b = this$memory[[action]][['b']]
        A_inv = solve(A)
        theta_hat = A_inv %*% b
        x_t = self$states[action]
        private$value_estimates[action] = t(x_t) %*% theta_hat
      }
    },

    observe = function(reward) {
      self$action_attempts[self$last_action] = self$action_attempts[self$last_action] + 1
      self$memory[self$last_action]['A'] = self$memory[self$last_action]['A'] + (self$states[self$last_action] %o% self$states[self$last_action])
      self$memory[self$last_action]['b'] = self$memory[self$last_action]['b'] + reward * matrix(self$states[self$last_action], self$d, 1)
      self$t = self$t + 1
    }

  ),
  private = list(
    value_estimates = NULL
  )
)
