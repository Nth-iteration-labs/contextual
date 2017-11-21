library(R6)

#' @export
Agent <- R6Class(

  "Agent",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    policy = NULL,
    bandit = NULL,
    k = NULL,
    prior = NULL,
    gamma = NA,
    alpha = NULL,
    baseline = NULL,
    action_attempts = NULL,
    t = NULL,
    last_action = NULL,
    init_exploration = NULL,
    average_reward = NULL,

    initialize = function(

      policy = NA,
      bandit = NA,
      prior = 0,
      alpha=0.1,
      baseline=TRUE,
      gamma= NA,
      k = NA,
      init_exploration = NA,
      average_reward = 0,
      d = 2

    ) {

      self$bandit = bandit
      self$policy = policy

      self$prior = prior
      self$alpha = alpha
      self$baseline = baseline

      self$k = 3 ###################################bandit$k
      self$init_exploration = init_exploration

      self$reset()
    },

    reset = function() {

      d = 2 ######################this should not be hardcoded here!

      # access as: theta[[1]][['A']][1,1]

      theta_elements = list('A' = diag(d),'b' = rep(0, 2)) #matrix(0,d,1) )
      for (i in 1:self$k) {
        private$theta[[i]] = theta_elements
      }

      self$action_attempts = rep(0, self$k)
      self$last_action = NA
      self$average_reward = 0

    },

    # This function updates the covariate matrix
    update_A = function(cov_matrix, context_vector){
      return(cov_matrix + context_vector %*% t(context_vector))
    },
    # this one updates b_a from above
    update_b = function(reward_vector_times_design_matrix, reward, context_vector){
      return(reward_vector_times_design_matrix + reward * context_vector)
    },

    get_action = function(context) {
      action = self$policy$get_action(self, context)  ############
      self$last_action = action
      return(action)
    },

    get_theta = function() {
        return(private$theta)
    },

    set_reward = function(arm, context) {  # this is not the reward, but the clicked arm .. intermingled ...

      private$theta[[ arm  ]][['A']] = update_A(private$theta[[ arm  ]][['A']],as.matrix(c(context$clicked_sports, context$clicked_politics)))

      private$theta[[ arm ]][['b']] = update_b(as.matrix(private$theta[[ arm ]][['b']]), reward, as.matrix(c(context$clicked_sports, context$clicked_politics)))

    }

  ),
  private = list(
    theta = NULL
  )
)
